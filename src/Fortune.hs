{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Fortune
import Data.Random hiding (Normal)
import Data.Random.Distribution.Categorical
import Data.Version
import qualified Data.Text as T
import Paths_misfortune
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Printf

versionString = "misfortune " ++ showVersion version
printVersion = do
    putStrLn versionString
    exitWith ExitSuccess

usage errors = do
    cmd <- getProgName
    
    mapM_ putStr errors
    when (not (null errors)) (putStrLn "")
    
    putStrLn versionString
    putStr (usageInfo (cmd ++ " [options] [files]") flags)
    
    exitWith (if null errors then ExitSuccess else ExitFailure 1)

data Flag = A | E | F | O | H | V deriving Eq

flags = 
    [ Option "a"  ["all"]       (NoArg A) "Use all fortune databases, even offensive ones"
    , Option "e"  []            (NoArg E) "Select fortune file with equal probability for all"
    , Option "f"  []            (NoArg F) "List the fortune files that would be searched"
    , Option "o"  ["offensive"] (NoArg O) "Use only the potentially-offensive databases"
    , Option "h?" ["help"]      (NoArg H) "Show this help message"
    , Option ""   ["version"]   (NoArg V) "Print version info and exit"
    ]

data Args = Args
    { equalProb         :: Bool
    , printDist         :: Bool
    , fortuneFiles      :: [FilePath]
    }

parseArgs = do
    (opts, files, errors) <- getOpt Permute flags <$> getArgs
    when (not (null errors))       (usage errors)
    when (H `elem` opts) (usage [])
    when (V `elem` opts) printVersion
    
    let fortuneType
            | A `elem` opts = All
            | O `elem` opts = Offensive
            | otherwise     = Normal
    
    fortuneFiles <- if null files
        then defaultFortuneFiles fortuneType
        else mapM (resolveFortuneFile All) files
    
    return Args
        { equalProb = E `elem` opts
        , printDist = F `elem` opts
        , ..
        }

main = do
    args <- parseArgs
    fortunes <- mapM (\f -> openFortuneFile f '%' False) (fortuneFiles args)
    
    when (null fortunes) (usage ["No fortunes specified"])
    
    weights  <- mapM (getWeight args) fortunes
    
    if printDist args
        then sequence_
            -- TODO: merge paths into a tree for nicer presentation
            [ printf "%7.2f%%: %s\n" (100 * fromIntegral weight / totalWeight) path
            | let totalWeight = fromIntegral (sum weights) :: Float
            , (path, weight) <- zip (fortuneFiles args) weights
            ]
        else do
            (i, j) <- sample (fortune weights)
            fortune <- getFortune (fortunes !! i) j
            putStrLn (T.unpack fortune)

getWeight args
    | equalProb args    = const (return 1)
    | otherwise         = getNumFortunes

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

