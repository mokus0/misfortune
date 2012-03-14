module Data.Fortune
     ( module Data.Fortune.FortuneFile
     , module Data.Fortune.Index
     , module Data.Fortune.Stats
     , module Data.Fortune
     ) where

import Data.Fortune.FortuneFile
import Data.Fortune.Index
import Data.Fortune.Stats

import Control.Applicative
import Data.List
import Paths_misfortune
import System.Directory
import System.FilePath
import System.Random

getFortuneDir = getDataDir

fortuneFilesIn recursive dir = do
    let hidden ('.':_)  = True
        hidden _        = False
    
    contents <- filter (not . hidden) <$> getDirectoryContents dir
    concat <$> sequence
        [ do 
            isFile   <- doesFileExist path
            hasIndex <- doesFileExist (path <.> "dat")
            if isFile
                then return [path | hasIndex]
                else if recursive 
                    then fortuneFilesIn recursive path
                    else return []
        | file <- contents
        , not (".dat" `isSuffixOf` file)
        , let path = dir </> file
        ]

defaultFortuneFiles allFortunes offensiveOnly = do
    dir <- getFortuneDir
    case (allFortunes, offensiveOnly) of
        (True,  _    ) -> fortuneFilesIn True dir
        (False, True ) -> concat <$> mapM (fortuneFilesIn False)
            [ dir </> "off"
            , dir </> "off" </> "lambdabot"
            , dir </> "off" </> "fortune-mod"
            ]
        (False, False) -> concat <$> mapM (fortuneFilesIn False)
            [ dir
            , dir </> "lambdabot"
            , dir </> "fortune-mod"
            ]

randomFortune f = do
    n <- getNumFortunes f
    i <- randomRIO (0, n-1)
    getFortune f i