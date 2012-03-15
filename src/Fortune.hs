{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Fortune
import Data.Random hiding (Normal)
import Data.Random.Distribution.Categorical
import Data.Version
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
        else do
            searchPath     <- getFortuneSearchPath fortuneType
            fullSearchPath <- getFortuneSearchPath All
            (missing, found) <- partitionEithers <$> mapM (resolve searchPath fullSearchPath) files
            if null missing
                then return (concat found)
                else usage ["Fortune database not found: " ++ file | file <- missing]
            
    
    return Args
        { equalProb = E `elem` opts
        , printDist = F `elem` opts
        , ..
        }

-- find a fortune file... first check the given search path.
-- If it's not there, check fullSearchPath.  Otherwise barf.
-- 
-- To see why we do this, consider these 2 cases:
--  1)  User says @misfortune foo@.  foo is an "offensive" fortune file.
--      We want the user to get what they asked for without needing "-o".
--  2)  User says @misfortone bar@,  bar has both "normal" and 
--      "offensive" fortune files.  We want the normal one but _NOT_ the
--      offensive one, because the user didn't say "-o".
resolve searchPath fullSearchPath file = do
    files <- findFortuneFileIn searchPath file
    if null files
        then do
            files <- if searchPath /= fullSearchPath
                then findFortuneFileIn fullSearchPath file
                else return []
            
            if null files
                then return (Left file)
                else return (Right files)
            
        else return (Right files)

main = do
    args <- parseArgs
    fortunes <- mapM (\f -> openFortuneFile f '%' False) (fortuneFiles args)
    
    when (null fortunes) (usage ["No fortunes specified"])
    
    dist <- getDist args fortunes
    
    if printDist args
        then sequence_
            -- TODO: merge paths into a tree for nicer presentation
            [ printf "%7.2f%%: %s\n" (100 * weight) (fortuneFilePath file)
            | (weight, file) <- toList dist
            ]
        else putStrLn =<< randomFortuneFromRandomFile (rvar dist)

getDist args files
    | equalProb args    = return ( fromWeightedList [ (1, f) | f <- files ])
    | otherwise         = defaultFortuneDistribution files
