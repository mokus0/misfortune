module Main where

import Data.Fortune
import System.Environment

main = do
    args <- getArgs
    mapM_ index args

index file = do
    fortune <- openFortuneFile '%' True file
    rebuildIndex fortune
    