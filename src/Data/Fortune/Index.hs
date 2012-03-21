{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- |Data structure, serialization, and file i/o for @strfile@-style index files
-- 
-- The old @strfile@ \"format\" has some serious funkiness, especially on 64-bit systems.
-- This is a saner implementation of the same concept.
--
-- The file format is as follows:
-- 
-- section | offset | format    | description
-- ========|========| ==========|==============
-- header  |      0 | word32be  | Magic number (0xbdcbcdb, a hard-to-type base-16 palindromic prime)
--         |      4 | word32be  | Version number (currently 2)
--         |      8 | word32be  | Offset of string table in index file
--         |     12 | word32be  | Number of entries in string table
--         |     16 | word32be  | Maximum number of chars in a string
--         |     20 | word32be  | Minimum number of chars in a string
--         |     24 | word32be  | Maximum number of lines in a string
--         |     28 | word32be  | Minimum number of lines in a string
--         |     32 | word32be  | Offset in string file after last char of last fortune
--         |     36 | 28 bytes  | reserved (set to 0 when not in use)
-- ========|========| ==========|==============
-- table   |     ?? | entry*    | Offset given in header.  Format given below.
--
-- entries are 16 bytes each, and consist of:
-- 
-- offset | format   | description
-- =======|==========|==============
--      0 | word32be | byte offset of string in file
--      4 | word32be | byte length of string in file
--      8 | word32be | number of characters in string
--     12 | word32be | number of lines in string
module Data.Fortune.Index
     ( Index
     , openIndex
     , createVirtualIndex
     , closeIndex
     , getStats
     
     , StatsProblem(..)
     , HeaderProblem(..)
     , IndexProblem(..)
     , checkIndex
     
     , IndexEntry(..)
     , indexEntryStats
     , getEntries
     , getEntry
     , unfoldEntries
     , appendEntries
     , appendEntry
     , clearIndex
     , rebuildStats
     ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable (foldMap)
import Data.Fortune.Stats
import Data.Knob
import Data.Maybe
import Data.Semigroup
import Data.Serialize
import Data.Typeable
import qualified Data.Vector as V
import Data.Word
import System.IO

runGetM getThing = either fail return . runGet getThing

magic, currentVersion :: Word32
magic                   = 0xbdcbcdb
currentVersion          = 2

headerLength            = 64 -- bytes
headerReservedLength    = 28 -- bytes

data Header = Header
    { stats     :: !FortuneStats
    , indexLoc  :: !Int
    } deriving (Eq, Show)

emptyHeader = Header mempty headerLength

-- |An exception type indicating things that can be wrong about an index file's header.
data HeaderProblem
    = BadMagicNumber !Word32
    | UnsupportedVersion !Word32
    | StatsProblem !StatsProblem
    | TableStartsBeforeHeaderEnds
    deriving (Eq, Ord, Read, Show, Typeable)

checkHeader (Header stats loc)
    | loc < headerLength    = Just TableStartsBeforeHeaderEnds
    | otherwise             = StatsProblem <$> checkStats stats

knownVersions = [(currentVersion, getRestV2)]

getHeader = do
    n <- getWord32be
    when (n /= magic) $ throw (BadMagicNumber n)
    version <- getWord32be
    case lookup version knownVersions of
        Just getRest -> getRest
        Nothing      -> throw (UnsupportedVersion version)

getRestV2 = do
    indexLoc    <-       fromIntegral <$> getWord32be
    numFortunes <- Sum . fromIntegral <$> getWord32be
    maxChars    <- Max . fromIntegral <$> getWord32be
    minChars    <- Min . fromIntegral <$> getWord32be
    maxLines    <- Max . fromIntegral <$> getWord32be
    minLines    <- Min . fromIntegral <$> getWord32be
    offsetAfter <- Max . fromIntegral <$> getWord32be
    skip headerReservedLength
    
    return Header {stats = FortuneStats{..}, ..}

putHeader Header {stats = FortuneStats{..}, ..} = do
    putWord32be magic
    putWord32be currentVersion
    putWord32be (fromIntegral indexLoc)
    putWord32be (fromIntegral (getSum numFortunes))
    putWord32be (fromIntegral (getMax maxChars))
    putWord32be (fromIntegral (getMin minChars))
    putWord32be (fromIntegral (getMax maxLines))
    putWord32be (fromIntegral (getMin minLines))
    putWord32be (fromIntegral (getMax offsetAfter))
    replicateM_ headerReservedLength (putWord8 0)

-- |A handle to an open fortune index file.
data Index = Index !Handle !(MVar Header)

-- |@openIndex path writeMode@: Opens the index file at @path@.  The 'Index' will
-- be writable if @writeMode@ is 'True'.  If there is no index file at that path, 
-- an error will be thrown or the index will be created, depending on @writeMode@.
openIndex :: FilePath -> Bool -> IO Index
openIndex path writeMode = do
    file <- openFile path (if writeMode then ReadWriteMode else ReadMode)
    openIndex' file writeMode

-- |Create an in-memory index - useful for working with files when, for whatever reason,
-- you cannot create a valid index.
createVirtualIndex :: IO Index
createVirtualIndex = do
    knob <- newKnob BS.empty
    file <- newFileHandle knob "<createVirtualIndex>" ReadWriteMode
    openIndex' file True

openIndex' :: Handle -> Bool -> IO Index
openIndex' file writeMode = do
    hSetBinaryMode file True
    hSetBuffering file NoBuffering
    
    isEmpty <- hIsEOF file
    when (writeMode && isEmpty) $ do
        BS.hPut file (runPut (putHeader emptyHeader))
        hSeek file AbsoluteSeek 0
        
    hdr <- BS.hGet file headerLength 
    
    case runGet getHeader hdr of
        Left err -> fail err
        Right hdr -> do
            -- check header for problems, fixing what we can and throwing what we can't
            mbProblem <- checkIndex_ file hdr
            case mbProblem of
                Just (HeaderProblem StatsProblem{}) -> void (rebuildStats_ file hdr)
                Just p                              -> throwIO p
                Nothing                             -> return ()
            
            hdrRef <- newMVar hdr
            return (Index file hdrRef)

-- |Close an index file.  Subsequent accesses will fail.
closeIndex :: Index -> IO ()
closeIndex (Index file mv) = do
    hClose file
    takeMVar mv
    putMVar mv (throw AccessToClosedIndex)

-- |Errors that can be thrown indicating a problem with an index file.
data IndexProblem
    = HeaderProblem !HeaderProblem
    | TableLongerThanFile
    | AccessToClosedIndex
    deriving (Eq, Ord, Read, Show, Typeable)

-- These instances allow any 'problem' to be caught as an instance of any other,
-- to the extent that that "makes sense"
instance Exception StatsProblem where
    fromException se@(SomeException e) = listToMaybe $ catMaybes
        [ cast e
        , do StatsProblem p <- fromException se; return p
        ]
instance Exception HeaderProblem where
    fromException se@(SomeException e) = listToMaybe $ catMaybes
        [ cast e
        , StatsProblem <$> fromException se
        , do HeaderProblem p <- fromException se; return p
        ]
instance Exception IndexProblem where
    fromException se@(SomeException e) = listToMaybe $ catMaybes
        [ cast e
        , HeaderProblem <$> fromException se
        ]

-- |Force a consistency check on an index file.
checkIndex :: Index -> IO (Maybe IndexProblem)
checkIndex (Index file hdrRef) =
    either Just id <$> try (withMVar hdrRef (checkIndex_ file))

-- TODO: also random spot-check for validity of table entries (mostly that they are consistent with the stats).
checkIndex_ file hdr =
    case checkHeader hdr of
        Just problem -> return (Just (HeaderProblem problem))
        Nothing -> do
            let base = indexLoc hdr
                count = numFortunes (stats hdr)
                end = base + getSum count * indexEntryLength
            len <- hFileSize file
            return $! if len < toInteger end
                then Just TableLongerThanFile
                else Nothing

withIndex ix@(Index file hdrRef) action = withMVar hdrRef $ \hdr -> do
    let base = indexLoc hdr
        count = numFortunes (stats hdr) 
    res <- action file base (getSum count)
    
    -- TODO: build flag to control paranoia level?
    checkIndex_ file hdr >>= maybe (return res) throwIO
    

modifyHeader (Index file hdrRef) action = modifyMVar_ hdrRef $ \hdr -> do
    newHdr <- action file hdr
    
    when (newHdr /= hdr) $ do
        hSeek file AbsoluteSeek 0
        BS.hPut file (runPut (putHeader newHdr))
    
    -- TODO: build flag to control paranoia level?
    checkIndex_ file newHdr >>= maybe (return newHdr) throwIO

-- |Get some cached stats about the fortunes indexed in this file.
getStats :: Index -> IO FortuneStats
getStats (Index _ hdrRef) = stats <$> readMVar hdrRef

indexEntryLength = 16 -- bytes

-- |Conceptually, an 'Index' file is just a header containing 'FortuneStats' and an array of these entries.
-- An 'IndexEntry' stores the information needed to locate one string in the fortune fiel, as well as some
-- basic stats about that one file (from which the 'FortuneStats' will be derived).
data IndexEntry = IndexEntry
    { stringOffset  :: !Int
        -- ^ The location of the string in the file, as a byte offset
    , stringBytes   :: !Int
        -- ^ The number of bytes the string occupies.
    , stringChars   :: !Int
        -- ^ The number of characters in the string.
    , stringLines   :: !Int
        -- ^ The number of lines in the string.
    } deriving (Eq, Ord, Show)

-- |Convert one index entry to a 'FortuneStats' record describing it.
indexEntryStats :: IndexEntry -> FortuneStats
indexEntryStats (IndexEntry o n cs ls) = FortuneStats
    { numFortunes = Sum 1, offsetAfter = Max (o + n)
    , minChars    = Min cs, maxChars    = Max cs
    , minLines    = Min ls, maxLines    = Max ls
    }

putIndexEntry IndexEntry{..} = do
    putWord32be (fromIntegral stringOffset)
    putWord32be (fromIntegral stringBytes)
    putWord32be (fromIntegral stringChars)
    putWord32be (fromIntegral stringLines)

getIndexEntry = do
    stringOffset <- fromIntegral <$> getWord32be
    stringBytes  <- fromIntegral <$> getWord32be
    stringChars  <- fromIntegral <$> getWord32be
    stringLines  <- fromIntegral <$> getWord32be
    return IndexEntry{..}

-- |Read all the entries in an 'Index'
getEntries :: Index -> IO (V.Vector IndexEntry)
getEntries ix = withIndex ix $ \file base count -> do
    hSeek file AbsoluteSeek (toInteger base)
    buf <- BS.hGet file (count * indexEntryLength)
    runGetM (V.replicateM count getIndexEntry) buf

-- |Read a specified entry from an 'Index'.
getEntry :: Index -> Int -> IO IndexEntry
getEntry ix@(Index file hdrRef) i
    | i < 0     = rangeErr
    | otherwise = withIndex ix $ \file base count -> do
        when (i >= count) rangeErr
        
        hSeek file AbsoluteSeek (toInteger (base + i * indexEntryLength))
        BS.hGet file indexEntryLength >>= runGetM getIndexEntry
    where rangeErr = fail ("getEntry: index out of range: " ++ show i)

-- |Repeatedly invoke a generator for index entries until it returns 'Nothing',
-- appending all entries returned to the index file.
unfoldEntries :: Index -> IO (Maybe IndexEntry) -> IO ()
unfoldEntries ix getEntry = modifyHeader ix $ \file hdr -> do
        let base = indexLoc hdr
            count = numFortunes (stats hdr)
            end = base + getSum count * indexEntryLength
            
            loop s = do
                mbEntry <- getEntry
                case mbEntry of
                    Nothing -> return s
                    Just entry -> do
                        BS.hPut file (runPut (putIndexEntry entry))
                        loop $! (s <> indexEntryStats entry)
        
        hSeek file AbsoluteSeek (toInteger end)
        newStats <- loop (stats hdr)
        
        return hdr {stats = newStats}

-- |Append all the given entries to the 'Index' file.
appendEntries :: Index -> V.Vector IndexEntry -> IO ()
appendEntries ix entries
    | V.null entries    = return ()
    | otherwise         = modifyHeader ix $ \file hdr -> do
        let base = indexLoc hdr
            count = numFortunes (stats hdr)
            end = base + getSum count * indexEntryLength
        
        hSeek file AbsoluteSeek (toInteger end)
        BS.hPut file (runPut (V.mapM_ putIndexEntry entries))
        
        return hdr {stats = stats hdr <> foldMap indexEntryStats entries}

-- |Append a single 'IndexEntry' to an 'Index' file.
appendEntry :: Index -> IndexEntry -> IO ()
appendEntry ix = appendEntries ix . V.singleton

-- |Delete all entries from an 'Index'.
clearIndex :: Index -> IO ()
clearIndex ix = modifyHeader ix $ \file _ -> do
    hSetFileSize file (toInteger headerLength)
    return emptyHeader

-- |All the operations here should preserve correctness of stats, but just in case...
-- This procedure forces the stats to be recomputed.
rebuildStats :: Index -> IO ()
rebuildStats ix = modifyHeader ix rebuildStats_

rebuildStats_ file hdr = do
    let n = getSum (numFortunes (stats hdr))
        chunk = 4096 `div` indexEntryLength
        loop i s
            | i >= n    = return s
            | otherwise = do
                let m = min chunk (n - i)
                entries <- runGetM (replicateM m getIndexEntry) =<< BS.hGet file (m * indexEntryLength)
                loop (i + chunk) (s <> foldMap indexEntryStats entries)
    
    hSeek file AbsoluteSeek (toInteger (indexLoc hdr))
    newStats <- loop 0 mempty
    
    return hdr {stats = newStats}
