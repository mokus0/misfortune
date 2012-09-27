{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Fortune.FortuneFile
     ( FortuneFile
     , fortuneFilePath
     , fortuneIndexPath
     , openFortuneFile
     , closeFortuneFile
     , getIndex
     , rebuildIndex
     , getFortune
     , getFortunes
     , getNumFortunes
     , appendFortune
     ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U
import Data.Fortune.Index
import Data.Fortune.Stats
import Data.IORef
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO

-- |A handle to an open fortune database.
data FortuneFile = FortuneFile 
    { fortunePath       :: !FilePath
    , fortuneDelim      :: !Char
    , fortuneWritable   :: !Bool
    , fortuneFile       :: !(MVar (Maybe Handle))
    , fortuneIndex      :: !(MVar (Maybe Index))
    }

-- |Get the path of the text part of an open fortune database.
fortuneFilePath :: FortuneFile -> FilePath
fortuneFilePath = fortunePath

-- |Get the path of the index part of an open fortune database.
fortuneIndexPath :: FortuneFile -> FilePath
fortuneIndexPath f = fortunePath f <.> "ix"

-- |@openFortuneFile path delim writeMode@: Open a fortune file at @path@,
-- using @delim@ as the character between strings, allowing writing if
-- @writeMode@ is set.  If no file exists at the specified path, an error
-- will be thrown or the file will be created, depending on @writeMode@.
openFortuneFile :: Char -> Bool -> FilePath -> IO FortuneFile
openFortuneFile fortuneDelim fortuneWritable fortunePath = do
    exists <- doesFileExist fortunePath
    when (not (exists || fortuneWritable))
        (fail ("openFortuneFile: file does not exist: " ++ show fortunePath))
    
    fortuneFile  <- newMVar Nothing
    fortuneIndex <- newMVar Nothing
    return FortuneFile{..}

-- |Close a fortune file. Subsequent accesses will fail.
closeFortuneFile :: FortuneFile -> IO ()
closeFortuneFile f = do
    maybe (return ()) hClose     =<< takeMVar (fortuneFile  f)
    putMVar (fortuneFile f) (error "Fortune file is closed")
    
    maybe (return ()) closeIndex =<< takeMVar (fortuneIndex f)
    putMVar (fortuneIndex f) (error "Fortune file is closed")

withFortuneFile f action = modifyMVar (fortuneFile f) $ \mbFile ->
    case mbFile of
        Nothing -> do
            file <- openFile (fortunePath f) (if fortuneWritable f then ReadWriteMode else ReadMode)
            res <- action file
            return (Just file, res)
        Just file -> do
            res <- action file
            return (Just file, res)

withIndex f action =
    modifyMVar (fortuneIndex f) $ \mbIx ->
        case mbIx of
            Nothing -> do
                let path      = fortuneIndexPath f
                    writeMode = fortuneWritable  f
                    -- if read-only, create an in-memory index if the real one exists but can't be opened
                    -- (Don't do that for read-write mode, because the writes would silently be dropped)
                    -- If building the in-memory one fails, re-throw the original exception; it's more
                    -- informative because it tells why the index couldn't be opened in the first place.
                    onExc e = if writeMode
                        then throwIO (e :: SomeException)
                        else handle (rethrow e) $ do
                            ix <- createVirtualIndex
                            withFortuneFile f (\file -> rebuildIndex' (fortuneDelim f) file ix)
                            return ix
                    rethrow e other = throwIO (e `asTypeOf` other)
                
                ix <- handle onExc (openIndex path writeMode)
                res <- action ix
                return (Just ix, res)
            Just ix -> do
                res <- action ix
                return (Just ix, res)


withFileAndIndex f action = withFortuneFile f (withIndex f . action)

-- |Get the 'Index' of a 'FortuneFile', opening it if necessary.
getIndex :: FortuneFile -> IO Index
getIndex fortunes = withIndex fortunes return

-- |Clear a 'FortuneFile's 'Index' and rebuild it from the contents 
-- of the text file.
rebuildIndex :: FortuneFile -> IO ()
rebuildIndex f = withFileAndIndex f (rebuildIndex' (fortuneDelim f))

rebuildIndex' delim file ix = do
    clearIndex ix
    hSeek file AbsoluteSeek 0
    
    getEntry <- enumFortuneLocs file delim
    unfoldEntries ix getEntry

-- |scan an open handle for UTF8 chars.  For each one found, returns the byte
-- location, the char, and the byte width of the char.
-- WARNING: seeks through file.  Do not perform any other IO on the same file until the returned thingy says "Nothing".
enumUTF8 :: Handle -> IO (IO (Maybe (Int, Char, Int)))
enumUTF8 file = do
    let getChunk = BS.hGet file 4096
        refill buf
            | BS.null buf   = getChunk
            | otherwise     = return buf
    
    bytePosRef <- hTell file >>= newIORef . fromInteger
    bufRef     <- getChunk >>= newIORef 
    
    let getOne = do
            buf <- readIORef bufRef
            if BS.null buf
                then return Nothing
                else case tryDecode buf of
                    Nothing -> do
                        -- this case occurs when there is a partial char at the
                        -- end of the buffer; check for more input; if there is none,
                        -- discard the partial char.
                        more <- getChunk
                        writeIORef bufRef $! if BS.null more
                            then BS.empty
                            else BS.append buf more
                        getOne
                    Just (c, n, rest) -> do
                        refill rest >>= writeIORef bufRef
                        bytePos <- readIORef bytePosRef
                        writeIORef bytePosRef $! bytePos + n
                        
                        return (Just (bytePos, c, n))
    
    return getOne

-- try to decode the first UTF-8 char in a buffer.  If the decoding fails 
-- (returns replacement_char), then check if the whole buffer was used.
-- if it was, we probably just need more data so return Nothing.
tryDecode bs = case U.decode bs of
    Just (c, n)
        | c /= U.replacement_char || n /= BS.length bs
            -> Just (c, n, BS.drop n bs)
    _       -> Nothing

-- WARNING: seeks through file.  Do not perform any other IO on the same file until the returned thingy says "Nothing".
enumFortuneLocs :: Handle -> Char -> IO (IO (Maybe IndexEntry))
enumFortuneLocs file delim = do
    curStart <- hTell file >>= newIORef . fromInteger
    prev     <- newIORef Nothing
    curBytes <- newIORef 0
    curChars <- newIORef 0
    curLines <- newIORef 0
    
    nextChar <- enumUTF8 file
    
    let nextFortune = do
            mbP <- readIORef prev
            mbC <- nextChar
            writeIORef prev mbC
            
            case (mbP, mbC) of
                (Nothing, Nothing) -> return Nothing
                (Just (_, p, pN),  Nothing)
                     | p == '\n'    -> emit pN 1
                     | otherwise    -> newline >> emit 0 0
                    
                (Just (_, p, pN), Just (_, c, n))
                    | p == '\n' && c == delim -> do
                        mbN <- nextChar
                        case mbN of 
                            Just (loc,'\n',n) -> emit pN 1 <* reset (loc + n)
                            _ -> advance n
                (_, Just (_, c, n)) -> do
                    when (c == '\n') newline
                    advance n
        newline = modifyIORef' curLines (1 +)
        advance n = do
            modifyIORef' curBytes (n +)
            modifyIORef' curChars (1 +)
            nextFortune
        reset loc = do
            writeIORef curStart $! loc
            writeIORef curBytes 0
            writeIORef curChars 0
            writeIORef curLines 0
        -- the params are the amount to 'rewind' to cut off the final
        -- newline in a quote, if necessary
        emit dB dC = do
            start <- readIORef curStart
            bytes <- readIORef curBytes
            chars <- readIORef curChars
            ls    <- readIORef curLines
                                
            return (Just (IndexEntry start (bytes - dB) (chars - dC) ls))
    
    return nextFortune

#if !MIN_VERSION_base(4,6,0)

modifyIORef' r f = do
    x <- readIORef r
    writeIORef r $! f x

#endif

getByIndex file (IndexEntry loc len _ _) = do
    hSeek file AbsoluteSeek (toInteger loc)
    BS.hGet file len

-- |@getFortune f i@ retrieves the text of the @i@'th fortune
-- (according to the order in the index file) in the 'FortuneFile' @f@.
getFortune :: FortuneFile -> Int -> IO T.Text
getFortune f i = do
    ix <- getIndex f
    entry <- getEntry ix i
    T.decodeUtf8With T.lenientDecode <$> 
        withFortuneFile f (flip getByIndex entry)

-- |Get the text of every fortune in a fortune file,
-- in the order they occur in the file.  Ignores the index
-- entirely.
getFortunes :: FortuneFile -> IO [T.Text]
getFortunes f = withFortuneFile f $ \file -> do
    hSeek file AbsoluteSeek 0
    T.splitOn (T.pack ['\n', fortuneDelim f, '\n']) <$> T.hGetContents file

-- |Get the number of fortunes in a fortune file, as recorded
-- in the index.
getNumFortunes :: FortuneFile -> IO Int
getNumFortunes f = do
    ix <- getIndex f
    getSum . numFortunes <$> getStats ix

-- |Append a fortune to a fortune file, inserting a delimiter if
-- needed and updating the index.
appendFortune :: FortuneFile -> T.Text -> IO ()
appendFortune f fortune = do
    rebuildIndex f
    withFileAndIndex f $ \file ix -> do
        offset <- max 0 . getMax . offsetAfter <$> getStats ix
        hSeek file AbsoluteSeek (toInteger offset)
        
        
        let enc = T.encodeUtf8
            sep | offset == 0   = BS.empty
                | otherwise     = enc (T.pack ['\n', fortuneDelim f, '\n'])
            encoded = enc fortune
        
        BS.hPut file sep
        BS.hPut file encoded
        BS.hPut file (enc (T.pack "\n")) 
            -- just to be nice to people with @cat@s
        
        hFlush file
        
        appendEntry ix IndexEntry
            { stringOffset  = offset + BS.length sep
            , stringBytes   = BS.length encoded
            , stringChars   = T.length fortune
            , stringLines   = length (T.lines fortune)
            }
