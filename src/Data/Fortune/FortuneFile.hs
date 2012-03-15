{-# LANGUAGE BangPatterns #-}
module Data.Fortune.FortuneFile
     ( FortuneFile
     , openFortuneFile
     , closeFortuneFile
     , fortuneFilePath
     , fortuneIndexPath
     , getIndex
     , rebuildIndex
     , getFortune
     , getFortunes
     , getNumFortunes
     , appendFortune
     ) where

import Control.Applicative
import Control.Concurrent
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

data FortuneFile = FortuneFile 
    { fortunePath       :: !FilePath
    , fortuneDelim      :: !Char
    , fortuneWritable   :: !Bool
    , fortuneFile       :: !(MVar (Maybe Handle))
    , fortuneIndex      :: !(MVar (Maybe Index))
    }

fortuneFilePath = fortunePath
fortuneIndexPath f = fortunePath f <.> "dat"

openFortuneFile path delim writeMode = do
    exists <- doesFileExist path
    when (not (exists || writeMode))
        (fail ("openFortuneFile: file does not exist: " ++ show path))
    
    fileRef <- newMVar Nothing
    ixRef   <- newMVar Nothing
    return FortuneFile
        { fortunePath       = path
        , fortuneDelim      = delim
        , fortuneWritable   = writeMode
        , fortuneFile       = fileRef
        , fortuneIndex      = ixRef
        }

closeFortuneFile f = do
    maybe (return ()) closeIndex =<< readMVar (fortuneIndex f)
    maybe (return ()) hClose     =<< readMVar (fortuneFile  f)

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
                ix <- openIndex (fortuneIndexPath f) (fortuneWritable f)
                res <- action ix
                return (Just ix, res)
            Just ix -> do
                res <- action ix
                return (Just ix, res)

withFileAndIndex f action = withFortuneFile f (withIndex f . action)

getIndex fortunes = withIndex fortunes return

rebuildIndex f = withFileAndIndex f $ \file ix -> do
    clearIndex ix
    hSeek file AbsoluteSeek 0
    
    getEntry <- enumFortuneLocs file (fortuneDelim f)
    unfoldEntries ix getEntry

-- scan an open handle for UTF8 chars.  For each one found, returns the byte
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

modifyIORef' r f = do
    x <- readIORef r
    writeIORef r $! f x

getByIndex file (IndexEntry loc len _ _) = do
    hSeek file AbsoluteSeek (toInteger loc)
    BS.hGet file len

getFortune f i = do
    ix <- getIndex f
    entry <- getEntry ix i
    T.decodeUtf8With T.lenientDecode <$> 
        withFortuneFile f (flip getByIndex entry)

getFortunes f = withFortuneFile f $ \file -> do
    hSeek file AbsoluteSeek 0
    T.splitOn (T.pack ['\n', fortuneDelim f, '\n']) <$> T.hGetContents file

getNumFortunes f = do
    ix <- getIndex f
    getSum . numFortunes <$> getStats ix

appendFortune f fortune = do
    -- TODO: detect whether a reindex is actually needed
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
