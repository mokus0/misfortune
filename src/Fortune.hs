{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Either
import Data.Fortune
import Data.List
import Data.Maybe
import Data.Random hiding (Normal)
import Data.Random.Distribution.Categorical
import qualified Data.Text as T
import qualified Data.Traversable as T
import Data.Version
import Paths_misfortune
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Printf
import Text.Regex.Base
import Text.Regex.PCRE

versionString = "misfortune " ++ showVersion version
printVersion = do
    putStrLn versionString
    exitWith ExitSuccess

printPath fortuneType = do
    path <- getFortuneSearchPath fortuneType
    let sign False dir@('+':_)  = '-':dir
        sign False dir          = dir
        sign True  dir          = '+':dir
    
    putStrLn (intercalate ":" [ sign rec dir | (dir, rec) <- path])
    exitWith ExitSuccess

usage errors = do
    cmd <- getProgName
    
    let isErr = not (null errors)
        out = if isErr then stderr else stdout
    
    mapM_ (hPutStrLn out) errors
    when isErr (hPutStrLn out "")
    
    hPutStrLn out versionString
    hPutStr   out (usageInfo (cmd ++ " [options] [files]") flags)
    
    exitWith (if isErr then ExitFailure 1 else ExitSuccess)

data Flag = A | D FilePath | E | F | I | L  | M String | S | LL Int | N Int | O | Path | H | V deriving Eq

flags = 
    [ Option "a"  ["all"]       (NoArg A)             "Use all fortune databases, even offensive ones"
    , Option "d"  ["dump"]      (ReqArg D "<path>")   "Dump all selected fortunes to a fortune file at <path>"
    , Option "e"  []            (NoArg E)             "Select fortune file with equal probability for all"
    , Option "f"  []            (NoArg F)             "List the fortune files that would be searched"
    , Option "i"  []            (NoArg I)             "Match the pattern given by -m case-insensitively"
    , Option "l"  ["long"]      (NoArg L)             "Print a long fortune"
    , Option "L"  []            (ReqArg ll "<n>")     "Consider fortunes with more than n lines to be \"long\""
    , Option "m"  []            (ReqArg  M "<regex>") "Restrict fortunes to those matching <regex>"
    , Option "n"  []            (ReqArg  n "<n>")     "Consider fortunes with more than n chars to be \"long\""
    , Option "s"  ["short"]     (NoArg S)             "Print a short fortune"
    , Option "o"  ["offensive"] (NoArg O)             "Use only the potentially-offensive databases"
    , Option "h?" ["help"]      (NoArg H)             "Show this help message"
    , Option ""   ["version"]   (NoArg V)             "Print version info and exit"
    , Option ""   ["path"]      (NoArg Path)          "Print the effective search path and exit"
    ] where
        rd x = case reads x of
            (y, ""):_ -> y
            _         -> error ("failed to parse command line option: " ++ show x)
        ll = LL . rd
        n  = N  . rd

data Threshold = Chars Int | Lines Int
defaultThreshold = Lines 2

data Length = Short | Long

type FortuneFilter = FortuneFile -> Maybe (Int, IndexEntry) -> IO Bool

data Args = Args
    { equalProb         :: Bool
    , printDist         :: Bool
    , dumpFortunes      :: Maybe FilePath
    , fortuneFilters    :: [FortuneFilter]
    , fortuneFiles      :: [FortuneFile]
    }

-- run all configured filters for an individual fortune
filterFile    args file     = andM [p file Nothing       | p <- fortuneFilters args]
filterFortune args file i e = andM [p file (Just (i, e)) | p <- fortuneFilters args]

parseArgs = do
    (opts, files, errors) <- getOpt Permute flags <$> getArgs
    when (not (null errors)) (usage (errors >>= lines))
    when (H `elem` opts) (usage [])
    when (V `elem` opts) printVersion
    
    let fortuneType
            | A `elem` opts = All
            | O `elem` opts = Offensive
            | otherwise     = Normal
    
    when (Path `elem` opts) (printPath fortuneType)
    
    fortuneFiles <- if null files
        then defaultFortuneFiles fortuneType
        else do
            searchPath     <- getFortuneSearchPath fortuneType
            fullSearchPath <- getFortuneSearchPath All
            (missing, found) <- partitionEithers <$> mapM (resolve searchPath fullSearchPath) files
            if null missing
                then return (concat found)
                else usage ["Fortune database not found: " ++ file | file <- missing]
    
    -- open them all
    fortuneFiles <- mapM (openFortuneFile '%' False) fortuneFiles
    
    return Args
        { equalProb = E `elem` opts
        , printDist = F `elem` opts
        , dumpFortunes = listToMaybe [ path | D path <- opts ]
        , fortuneFilters = parseFilters opts
        , ..
        }

parseFilters opts = mapMaybe (parseFilter opts) opts
parseFilter opts opt = case opt of
    L      -> Just (filterLength Long)
    S      -> Just (filterLength Short)
    (M rx) -> Just (filterRegex (mkRegex rx))
    _      -> Nothing
    where
        filterLength len _ (Just (_, e)) = return (checkThreshold threshold len (indexEntryStats e))
        filterLength len f Nothing       = checkThreshold threshold len <$> (getStats =<< getIndex f)
        
        filterRegex   rx f (Just (i, _)) = matchTest rx . T.unpack <$> getFortune f i
        filterRegex   _  _ Nothing       = return True
        
        mkRegex :: String -> Regex
        mkRegex = makeRegexOpts (compUTF8 + caseOpt) execBlank
        
        caseOpt = if I `elem` opts then compCaseless else 0
        threshold = fromMaybe defaultThreshold (listToMaybe (opts >>= f))
        f (LL n) = [Lines n]; f (N  n) = [Chars n]; f _ = []


-- longest one is long
checkThreshold t Long  s =      overThreshold t (maxChars s) (maxLines s)
-- shortest one is not long
checkThreshold t Short s = not (overThreshold t (minChars s) (minLines s))

overThreshold (Chars n) c l = c > n
overThreshold (Lines n) c l = l > n


-- find a fortune file... 2 main cases:
-- 1) the path is a simple name (contains no /'s):
--      first check the given search path.
--      If it's not there, check fullSearchPath.  Otherwise barf.
-- 
--      To see why we do this, consider these 2 cases:
--       1)  User says @misfortune foo@.  foo is an "offensive" fortune file.
--           We want the user to get what they asked for without needing "-o".
--       2)  User says @misfortone bar@,  bar has both "normal" and 
--           "offensive" fortune files.  We want the normal one but _NOT_ the
--           offensive one, because the user didn't say "-o".
-- 2) the path is not a simple name (contains at least one /):
--      Just check for the file.
resolve searchPath fullSearchPath file
    | any (`elem` pathSeparators) file = do
        exists <- doesFileExist file
        return $! if exists
            then Right [file]
            else Left file
    
    | otherwise = do
        files <- findFortuneFileIn searchPath file
        if null files
            then do
                files <- if searchPath /= fullSearchPath
                    then findFortuneFileIn fullSearchPath file
                    else return []
                
                return $! if null files
                    then Left file
                    else Right files
                
            else return (Right files)

main = do
    args <- parseArgs
    -- pre-filter files that cannot possibly match.  saves time filtering fortunes.
    fortunes <- filterM (filterFile args) (fortuneFiles args)
    
    dist <- getDist args fortunes
    
    when (numEvents dist == 0) $ do
        hPutStrLn stderr "No fortunes matched the filter criteria"
        exitWith (ExitFailure 2)
    
    case dumpFortunes args of
        Nothing -> return ()
        Just outPath -> do
            out <- openFortuneFile '%' True outPath
            
            sequence_
                [ getFortune file i >>= appendFortune out
                | (file, iDist) <- snd <$> toList dist
                , i             <- snd <$> toList iDist
                ]
            
            exitWith ExitSuccess
    
    if printDist args
        then sequence_
            [ printf "%5d %8s: %s\n" (numEvents iDist) pctStr (fortuneFilePath file)
            | (weight, (file, iDist)) <- toList dist
            , let pctStr = printf "(%.2f%%)" (100 * weight / totalWeight dist) :: String
            ]
        else do
            (file, fortuneDist) <- sample dist
            fortune <- sample fortuneDist
            putStrLn . T.unpack =<< getFortune file fortune

getDist :: Args -> [FortuneFile] -> IO (Categorical Float (FortuneFile, Categorical Float Int))
getDist args files = equalize <$> case fortuneFilters args of
    [] -> do
        dist <- defaultFortuneDistribution files
        let f file = do
                n <- getNumFortunes file
                return (file, fromObservations [0 .. n-1])
        T.mapM f dist
    _ -> fortuneDistributionWhere (filterFortune args) files
    where 
        equalize
            | equalProb args = mapCategoricalPs (const 1)
            | otherwise      = id
