module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Fortune
import Data.Random
import Data.Random.Distribution.Categorical
import qualified Data.Text as T
import Paths_misfortune
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

fortuneFilesIn dir = 
    filterM doesFileExist
        . map (dir </>)
        . filter (not . isSuffixOf ".dat")
        =<< getDirectoryContents dir

defaultFiles = do
    dir <- getDataDir
    concat <$> mapM fortuneFilesIn
         [ dir
         , dir </> "lambdabot"
         , dir </> "fortune-mod"
         ]

defaultSearchPath = do
    dir <- getDataDir
    return
        [ "."
        , dir
        , dir </> "lambdabot"
        , dir </> "fortune-mod"
        , dir </> "fortune-mod" </> "off"
        ]

usage = do
    putStrLn "Use the source, luke!"
    exitWith (ExitFailure 1)

main = do
    args <- getArgs
    files <- if null args 
        then defaultFiles 
        else do
            searchPath <- getFortuneSearchPath
            mapM (resolve searchPath) args
    
    fortunes <- mapM (\f -> openFortuneFile f '%' False) files
    counts   <- mapM getNumFortunes fortunes
    
    when (null fortunes) usage
    (i, j) <- sample (fortune counts)
    fortune <- getFortune (fortunes !! i) j
    putStrLn (T.unpack fortune)

getEnv' key = lookup key <$> getEnvironment
getFortuneSearchPath = getEnv' "MISFORTUNE_PATH" >>= maybe defaultSearchPath (return . chop)
    where
        chop "" = []
        chop str = case break (':' ==) str of
            (x, y) -> x : chop (dropWhile (':' ==) y)

resolve dirs file
    | isAbsolute file   = return file
    | otherwise         = search dirs
    where
        -- TODO: list files that would have matched if the index file had existed
        search [] = fail ("Could not find fortune file: " ++ show file)
        search (dir:dirs) = do
            let path = dir </> file
            exists      <- doesFileExist path
            indexExists <- doesFileExist (path <.> "dat")
            if exists && indexExists
                then return path
                else search dirs

fortune [] = return (0,0)
fortune [n] = do
    j <- uniform 0 (n-1)
    return (0, j)
fortune cs = do
    (i, n) <- sample $ fromWeightedList
         [ (fromIntegral c :: Float, (i, c))
         | (i, c) <- zip [0..] cs
         ]
    j <- uniform 0 (n-1)
    return (i, j)

