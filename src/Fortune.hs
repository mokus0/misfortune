{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Fortune
import Data.Maybe
import Data.Random hiding (Normal)
import Data.Random.Distribution.Categorical
import qualified Data.Text as T
import Data.Version
import Paths_misfortune
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

versionString = "misfortune " ++ showVersion version
printVersion = do
    putStrLn versionString
    exitWith ExitSuccess

usage errors = do
    cmd <- getProgName
    
    let isErr = not (null errors)
        out = if isErr then stderr else stdout
    
    mapM_ (hPutStr out) errors
    when isErr (hPutStrLn out "")
    
    hPutStrLn out versionString
    hPutStr   out (usageInfo (cmd ++ " [options] [files]") flags)
    
    exitWith (if isErr then ExitFailure 1 else ExitSuccess)

data Flag = A | E | F | L | S | LL Int | N Int | O | H | V deriving Eq

flags = 
    [ Option "a"  ["all"]       (NoArg A)       "Use all fortune databases, even offensive ones"
    , Option "e"  []            (NoArg E)       "Select fortune file with equal probability for all"
    , Option "f"  []            (NoArg F)       "List the fortune files that would be searched"
    , Option "l"  ["long"]      (NoArg L)       "Print a long fortune"
    , Option "L"  []            (ReqArg ll "n") "Consider fortunes with more than n lines to be \"long\""
    , Option "n"  []            (ReqArg  n "n") "Consider fortunes with more than n chars to be \"long\""
    , Option "s"  ["short"]     (NoArg S)       "Print a short fortune"
    , Option "o"  ["offensive"] (NoArg O)       "Use only the potentially-offensive databases"
    , Option "h?" ["help"]      (NoArg H)       "Show this help message"
    , Option ""   ["version"]   (NoArg V)       "Print version info and exit"
    ] where
        rd x = case reads x of
            (y, ""):_ -> y
            _         -> error ("failed to parse command line option: " ++ show x)
        ll = LL . rd
        n  = N  . rd

data Threshold = Chars Int | Lines Int
defaultThreshold = Lines 2

data Length = Short | Long

data Args = Args
    { equalProb         :: Bool
    , printDist         :: Bool
    , lengthRestriction :: Maybe Length
    , threshold         :: Threshold
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
        
        lengthRestriction = listToMaybe (opts >>= f)
            where f L = [Long]; f S = [Short]; f _ = []
        threshold = fromMaybe defaultThreshold (listToMaybe (opts >>= f))
            where f (LL n) = [Lines n]; f (N  n) = [Chars n]; f _ = []
    
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
    fortunes <- filterFortuneFiles args fortunes
    when (null fortunes) $ do
        hPutStrLn stderr "No fortunes matched the length restriction"
        exitWith (ExitFailure 2)
    
    dist <- getDist args fortunes
    if printDist args
        then sequence_
            -- TODO: merge paths into a tree for nicer presentation
            [ printf "%5d %8s: %s\n" n pctStr (fortuneFilePath file)
            | (weight, (file, n, _)) <- toList dist
            , let pctStr = printf "(%.2f%%)" (100 * weight / totalWeight dist) :: String
            ]
        else do
            (file, _, fortuneDist) <- sample dist
            fortune <- sample fortuneDist
            -- putStrLn (fortuneFilePath file ++ " - " ++ show fortune)
            putStrLn . T.unpack =<< getFortune file fortune

filterFortuneFiles args =
    maybe return
        (\r -> filterM (fmap (checkThreshold (threshold args) r) . getStats <=< getIndex))
        (lengthRestriction args)

-- longest one is long
checkThreshold t Long  s =      overThreshold t (maxChars s) (maxLines s)
-- shortest one is not long
checkThreshold t Short s = not (overThreshold t (minChars s) (minLines s))

overThreshold (Chars n) c l = c > n
overThreshold (Lines n) c l = l > n

getDist :: Args -> [FortuneFile] -> IO (Categorical Float (FortuneFile, Int, RVar Int))
getDist args files = equalize <$> case lengthRestriction args of
    Nothing -> fromWeightedList <$> sequence
        [ do
            n <- getNumFortunes f
            return ( fromIntegral n
                   , (f, n, uniform 0 (n-1))
                   )
        | f <- files
        ]
    Just r -> fromWeightedList <$> sequence
    -- TODO: maybe keep a quantile table in the index and use that here
    -- or maybe just try picking fortunes (rejecting those that don't meet the 
    -- requirements) and use this method for "-f" and as a fall back after some
    -- number of rejected fortunes
        [ do
            is <- filterFortunes (checkThreshold (threshold args) r . indexEntryStats) f
            let n = length is
            return ( fromIntegral n
                   , (f, n, rvar (fromObservations is :: Categorical Float Int))
                   )
        | f <- files
        ]
    where 
        equalize
            | equalProb args = mapCategoricalPs (const 1)
            | otherwise      = id
