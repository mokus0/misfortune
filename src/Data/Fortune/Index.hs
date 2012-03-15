{-# LANGUAGE DeriveDataTypeable #-}

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
--         |     16 | word32be  | Number of characters in longest string
--         |     20 | word32be  | Number of characters in shortest string
--         |     24 | word32be  | Number of lines in longest string
--         |     28 | word32be  | Number of lines in shortest string
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
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Typeable
import qualified Data.Vector as V
import Data.Word
import System.IO

runGetM getThing = either fail return . runGet getThing

magic, currentVersion :: Word32
magic = 0xbdcbcdb
currentVersion = 2

headerLength = 32 -- bytes

data Header = Header
    { stats     :: !FortuneStats
    , indexLoc  :: !Int
    } deriving (Eq, Show)

emptyHeader = Header mempty headerLength

data HeaderProblem
    = StatsProblem StatsProblem
    | TableStartsBeforeHeaderEnds
    deriving (Eq, Ord, Read, Show, Typeable)

checkHeader (Header stats loc)
    = case checkStats stats of
        Just problem -> Just (StatsProblem problem)
        Nothing
            | loc < headerLength    -> Just TableStartsBeforeHeaderEnds
            | otherwise             -> Nothing

knownVersions = [(currentVersion, getRestV2)]

getHeader = do
    n <- getWord32be
    when (n /= magic) $ fail "Expected magic number 0x0bdcbcdb at start of index file"
    version <- getWord32be
    case lookup version knownVersions of
        Just getRest -> getRest
        Nothing      -> fail ("Unsupported index file format version: " ++ show version)

getRestV2 = do
    tblOffset  <- getWord32be
    numStrings <- getWord32be
    mxc        <- getWord32be
    mnc        <- getWord32be
    mxl        <- getWord32be
    mnl        <- getWord32be
    return Header
        { stats     = FortuneStats
            { numFortunes   = fromIntegral numStrings
            , maxChars     = if numStrings == 0 then Nothing else Just (fromIntegral mxc)
            , minChars     = if numStrings == 0 then Nothing else Just (fromIntegral mnc)
            , maxLines     = if numStrings == 0 then Nothing else Just (fromIntegral mxl)
            , minLines     = if numStrings == 0 then Nothing else Just (fromIntegral mnl)
            }
        , indexLoc = fromIntegral tblOffset
        }

putHeader (Header (FortuneStats numStrings mxc mnc mxl mnl) tblOffset) = do
    putWord32be magic
    putWord32be currentVersion
    putWord32be (fromIntegral tblOffset)
    putWord32be (fromIntegral numStrings)
    putWord32be (maybe 0 fromIntegral mxc)
    putWord32be (maybe 0 fromIntegral mnc)
    putWord32be (maybe 0 fromIntegral mxl)
    putWord32be (maybe 0 fromIntegral mnl)
    putWord64be 0

data Index = Index !Handle !(MVar Header)

openIndex path writeMode = do
    file <- openFile path (if writeMode then ReadWriteMode else ReadMode)
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

closeIndex (Index file _) = hClose file

data IndexProblem
    = HeaderProblem HeaderProblem
    | TableLongerThanFile
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

checkIndex (Index file hdrRef) = withMVar hdrRef (checkIndex_ file)

-- TODO: also random spot-check for validity of table entries (mostly that they are consistent with the stats).
checkIndex_ file hdr =
    case checkHeader hdr of
        Just problem -> return (Just (HeaderProblem problem))
        Nothing -> do
            let base = indexLoc hdr
                count = numFortunes (stats hdr)
                end = base + count * indexEntryLength
            len <- hFileSize file
            return $! if len < toInteger end
                then Just TableLongerThanFile
                else Nothing

withIndex ix@(Index file hdrRef) action = withMVar hdrRef $ \hdr -> do
    let base = indexLoc hdr
        count = numFortunes (stats hdr) 
    res <- action file base count
    
    -- TODO: build flag to control paranoia level?
    checkIndex_ file hdr >>= maybe (return res) throwIO
    

modifyHeader (Index file hdrRef) action = modifyMVar_ hdrRef $ \hdr -> do
    newHdr <- action file hdr
    
    when (newHdr /= hdr) $ do
        hSeek file AbsoluteSeek 0
        BS.hPut file (runPut (putHeader newHdr))
    
    -- TODO: build flag to control paranoia level?
    checkIndex_ file newHdr >>= maybe (return newHdr) throwIO

getStats (Index _ hdrRef) = stats <$> readMVar hdrRef

indexEntryLength = 16 -- bytes

data IndexEntry = IndexEntry
    { stringOffset  :: Int
    , stringBytes   :: Int
    , stringChars   :: Int
    , stringLines   :: Int
    } deriving (Eq, Ord, Show)

indexEntryStats (IndexEntry _ _ cs ls) = FortuneStats 1 (Just cs) (Just cs) (Just ls) (Just ls)

putIndexEntry entry = do
    putWord32be (fromIntegral (stringOffset entry))
    putWord32be (fromIntegral (stringBytes  entry))
    putWord32be (fromIntegral (stringChars  entry))
    putWord32be (fromIntegral (stringLines  entry))

getIndexEntry = do
    off <- fromIntegral <$> getWord32be
    bs  <- fromIntegral <$> getWord32be
    cs  <- fromIntegral <$> getWord32be
    ls  <- fromIntegral <$> getWord32be
    return (IndexEntry off bs cs ls)

getEntries ix = withIndex ix $ \file base count -> do
    hSeek file AbsoluteSeek (toInteger base)
    buf <- BS.hGet file (count * indexEntryLength)
    runGetM (V.replicateM count getIndexEntry) buf

getEntry ix@(Index file hdrRef) i
    | i < 0     = rangeErr
    | otherwise = withIndex ix $ \file base count -> do
        when (i >= count) rangeErr
        
        hSeek file AbsoluteSeek (toInteger (base + i * indexEntryLength))
        BS.hGet file indexEntryLength >>= runGetM getIndexEntry
    where rangeErr = fail ("getEntry: index out of range: " ++ show i)

unfoldEntries ix getEntry = modifyHeader ix $ \file hdr -> do
        let base = indexLoc hdr
            count = numFortunes (stats hdr)
            end = base + count * indexEntryLength
            
            loop s = do
                mbEntry <- getEntry
                case mbEntry of
                    Nothing -> return s
                    Just entry -> do
                        BS.hPut file (runPut (putIndexEntry entry))
                        loop $! (s `mappend` indexEntryStats entry)
        
        hSeek file AbsoluteSeek (toInteger end)
        newStats <- loop (stats hdr)
        
        return hdr {stats = newStats}

appendEntries ix entries
    | V.null entries    = return ()
    | otherwise         = modifyHeader ix $ \file hdr -> do
        let base = indexLoc hdr
            count = numFortunes (stats hdr)
            end = base + count * indexEntryLength
        
        hSeek file AbsoluteSeek (toInteger end)
        BS.hPut file (runPut (V.mapM_ putIndexEntry entries))
        
        return hdr {stats = stats hdr `mappend` foldMap indexEntryStats entries}

appendEntry ix = appendEntries ix . V.singleton

clearIndex ix = modifyHeader ix $ \file _ -> do
    hSetFileSize file (toInteger headerLength)
    return emptyHeader

-- all the operations above should preserve correctness of stats, but just in case...
-- you can fix the stats with this procedure.
rebuildStats ix = modifyHeader ix rebuildStats_

rebuildStats_ file hdr = do
    let n = numFortunes (stats hdr)
        chunk = 4096 `div` indexEntryLength
        loop i s
            | i >= n    = return s
            | otherwise = do
                let m = min chunk (n - i)
                entries <- runGetM (replicateM m getIndexEntry) =<< BS.hGet file (m * indexEntryLength)
                loop (i + chunk) (s `mappend` foldMap indexEntryStats entries)
    
    hSeek file AbsoluteSeek (toInteger (indexLoc hdr))
    newStats <- loop 0 mempty
    
    return hdr {stats = newStats}
