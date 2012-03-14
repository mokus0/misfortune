module Data.Fortune
     ( module Data.Fortune.FortuneFile
     , module Data.Fortune.Index
     , module Data.Fortune.Stats
     
     , listFortuneFiles
     , findFortuneFile
     
     , FortuneType(..)
     , getFortuneDir
     , defaultFortuneFiles
     , defaultFortuneFileNames
     
     , resolveFortuneFile
     , randomFortune
     ) where

import Data.Fortune.FortuneFile
import Data.Fortune.Index
import Data.Fortune.Stats

import Control.Applicative
import Control.Exception
import Data.IORef
import Data.Monoid hiding (All)
import Data.Random hiding (Normal)
import qualified Data.Text as T
import Paths_misfortune
import System.Directory
import System.FilePath

-- list the full paths of all visible items in the given directory
listDir dir =
    map (dir </>) . filter (not . hidden) <$> getDirectoryContents dir
    where hidden name = take 1 name == "."

traverseDir onDir onFile dir = 
    mconcat <$> (mapM onItem =<< listDir dir)
    where onItem path = do
            isDir <- doesDirectoryExist path
            if isDir then onDir path else onFile path
    

listFortuneFiles recursive = search
    where
        search = traverseDir onDir onFile
        onDir dir
             | recursive    = search dir
             | otherwise    = return []
        onFile path
            | takeExtension path == ".dat"  = return []
            | otherwise = do
                hasIndex <- doesFileExist (path <.> "dat")
                return [path | hasIndex ]

findFortuneFile recursive file dir = do
    done <- newIORef False
    
    let search = traverseDir onDir onFile
        
        onDir dir = do
            isDone <- readIORef done
            if recursive && not isDone
                then search dir
                else return mempty
        
        onFile path = do
            isDone <- readIORef done
            if isDone || takeFileName path /= file
                then return mempty
                else do
                    hasIndex <- doesFileExist (path <.> "dat")
                    if hasIndex
                        then do
                            writeIORef done True
                            return (First (Just path))
                        else return mempty
    
    getFirst <$> search dir

data FortuneType
    = All
    | Normal
    | Offensive
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

getFortuneDir fortuneType = do
    dir <- getDataDir
    return $! case fortuneType of
        All         -> dir
        Normal      -> dir </> "normal"
        Offensive   -> dir </> "offensive"

defaultFortuneFiles fortuneType = 
    getFortuneDir fortuneType >>= listFortuneFiles True

defaultFortuneFileNames fType = map takeFileName <$> defaultFortuneFiles fType

resolveFortuneFile fType path
    | isAbsolute path   = return path
    | otherwise         = do
        exists <- doesFileExist path
        if exists
            then return path
            else do
                mbPath <- findFortuneFile True path =<< getFortuneDir fType
                maybe
                    (fail ("couldn't find fortune file: " ++ path))
                    return mbPath

randomFortune path = do
    path <- resolveFortuneFile All path
    bracket (openFortuneFile path '%' False) closeFortuneFile $ \f -> do
        n <- getNumFortunes f
        i <- sample (uniform 0 (n-1))
        T.unpack <$> getFortune f i
