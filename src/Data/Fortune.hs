module Data.Fortune
     ( module Data.Fortune.FortuneFile
     , module Data.Fortune.Index
     , module Data.Fortune.Stats
     , module Data.Fortune
     ) where

import Data.Fortune.FortuneFile
import Data.Fortune.Index
import Data.Fortune.Stats

import System.Random

randomFortune f = do
    n <- getNumFortunes f
    i <- randomRIO (0, n-1)
    getFortune f i