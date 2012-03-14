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
     , resolveFortuneFiles
     , randomFortune
     , randomFortuneFromRandomFile
     , defaultFortuneDistribution
     
     , withFortuneFile
     , withFortuneFiles
     ) where

import Data.Fortune.FortuneFile
import Data.Fortune.Index
import Data.Fortune.Stats

import Control.Applicative
import Control.Exception
import Data.Function
import Data.Random hiding (Normal)
import Data.Random.Distribution.Categorical
import qualified Data.Text as T
import Paths_misfortune
import System.Directory
import System.FilePath

-- list the full paths of all visible items in the given directory
listDir dir =
    map (dir </>) . filter (not . hidden) <$> getDirectoryContents dir
    where hidden name = take 1 name == "."

traverseDir onFile = fix $ \search dir ->
    let onItem path = do
            isDir <- doesDirectoryExist path
            if isDir then search path else onFile path
     in concat <$> (mapM onItem =<< listDir dir)

listFortuneFiles = traverseDir onFile
    where onFile path
            | takeExtension path == ".dat"  = return []
            | otherwise = do
                hasIndex <- doesFileExist (path <.> "dat")
                return [path | hasIndex ]

findFortuneFile file = traverseDir onFile
    where onFile path 
            | takeFileName path /= file = return []
            | otherwise = do
                hasIndex <- doesFileExist (path <.> "dat")
                return [ path | hasIndex ]

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
    getFortuneDir fortuneType >>= listFortuneFiles

defaultFortuneFileNames fType = map takeFileName <$> defaultFortuneFiles fType

resolveFortuneFile fType path
    | isAbsolute path   = return [path]
    | otherwise         = do
        exists <- doesFileExist path
        hasIndex <- doesFileExist (path <.> "dat")
        otherOccurrences <- case splitPath path of
            [_] -> findFortuneFile path =<< getFortuneDir fType
            _   -> return []
        
        return ([ path | exists && hasIndex ] ++ otherOccurrences)

resolveFortuneFiles fType = fmap concat . mapM (resolveFortuneFile fType)

randomFortune [] = do
    paths <- defaultFortuneFiles Normal
    if null paths
        then return "Very few profundities can be expressed in less than 80 characters."
        else randomFortune paths

randomFortune paths = withFortuneFiles paths '%' False $ \fs -> do
    randomFortuneFromRandomFile . rvar =<< defaultFortuneDistribution fs

randomFortuneFromRandomFile :: RVar FortuneFile -> IO String
randomFortuneFromRandomFile file = do
    f <- sample file
    n <- getNumFortunes f
    i <- sample (uniform 0 (n-1))
    T.unpack <$> getFortune f i

defaultFortuneDistribution :: [FortuneFile] -> IO (Categorical Float FortuneFile)
defaultFortuneDistribution [] = fail "defaultFortuneDistribution: no fortune files"
defaultFortuneDistribution fs = fromWeightedList <$> sequence
    [ do
        weight <- getNumFortunes f
        return (fromIntegral weight, f)
    | f <- fs
    ]

withFortuneFile path delim writeMode = 
    bracket (openFortuneFile path delim writeMode)
             closeFortuneFile

withFortuneFiles [] _ _ action = action []
withFortuneFiles (p:ps) delim writeMode action =
    withFortuneFile  p  delim writeMode $ \p ->
        withFortuneFiles ps delim writeMode $ \ps ->
            action (p:ps)
