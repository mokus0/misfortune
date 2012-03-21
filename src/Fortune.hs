{-# LANGUAGE RecordWildCards #-}
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
    
    mapM_ (hPutStr out) errors
    when isErr (hPutStrLn out "")
    
    hPutStrLn out versionString
    hPutStr   out (usageInfo (cmd ++ " [options] [files]") flags)
    
    exitWith (if isErr then ExitFailure 1 else ExitSuccess)

data Flag = A | E | F | I | L  | M String | S | LL Int | N Int | O | Path | H | V deriving Eq

flags = 
    [ Option "a"  ["all"]       (NoArg A)       "Use all fortune databases, even offensive ones"
    , Option "e"  []            (NoArg E)       "Select fortune file with equal probability for all"
    , Option "f"  []            (NoArg F)       "List the fortune files that would be searched"
    , Option "i"  []            (NoArg I)       "Match the pattern given by -m case-insensitively"
    , Option "l"  ["long"]      (NoArg L)       "Print a long fortune"
    , Option "L"  []            (ReqArg ll num) "Consider fortunes with more than n lines to be \"long\""
    , Option "m"  []            (ReqArg  M  rx) "Restrict fortunes to those matching <regex>"
    , Option "n"  []            (ReqArg  n num) "Consider fortunes with more than n chars to be \"long\""
    , Option "s"  ["short"]     (NoArg S)       "Print a short fortune"
    , Option "o"  ["offensive"] (NoArg O)       "Use only the potentially-offensive databases"
    , Option "h?" ["help"]      (NoArg H)       "Show this help message"
    , Option ""   ["version"]   (NoArg V)       "Print version info and exit"
    , Option ""   ["path"]      (NoArg Path)    "Print the effective search path and exit"
    ] where
        rd x = case reads x of
            (y, ""):_ -> y
            _         -> error ("failed to parse command line option: " ++ show x)
        ll = LL . rd
        n  = N  . rd
        num = "<n>"; rx = "<regex>"
        

data Threshold = Chars Int | Lines Int
defaultThreshold = Lines 2

data Length = Short | Long

type FortuneFilter = FortuneFile -> Int -> IndexEntry -> IO Bool

data Args = Args
    { equalProb         :: Bool
    , printDist         :: Bool
    , fortuneFilters    :: [FortuneFilter]
    , threshold         :: Threshold
    , fortuneFiles      :: [FortuneFile]
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
    
    when (Path `elem` opts) (printPath fortuneType)
    
    let threshold = fromMaybe defaultThreshold (listToMaybe (opts >>= f))
            where f (LL n) = [Lines n]; f (N  n) = [Chars n]; f _ = []
        
        filterLength len _ _ e = return (checkThreshold threshold len (indexEntryStats e))
        filterRegex   rx f i _ = matchTest rx . T.unpack <$> getFortune f i
        
        mkRegex :: String -> Regex
        mkRegex = makeRegexOpts (compUTF8 + caseOpt) execBlank
            where caseOpt = if I `elem` opts then compCaseless else 0
        
        mkFilter L      = Just (filterLength Long)
        mkFilter S      = Just (filterLength Short)
        mkFilter (M rx) = Just (filterRegex (mkRegex rx))
        mkFilter _      = Nothing
    
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
    fortuneFiles <- mapM (\f -> openFortuneFile f '%' False) fortuneFiles
    
    -- pre-filter files based on stats in the headers.  saves time filtering fortunes.
    let lengthRestriction = listToMaybe (opts >>= f)
            where f L = [Long]; f S = [Short]; f _ = []
    fortuneFiles <- filterFortuneFiles threshold lengthRestriction fortuneFiles
    
    return Args
        { equalProb = E `elem` opts
        , printDist = F `elem` opts
        , fortuneFilters = catMaybes (map mkFilter opts)
        , ..
        }

filterFortuneFiles threshold =
    maybe return
        (\r -> filterM (fmap (checkThreshold threshold r) . getStats <=< getIndex))

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
resolve searchPath fullSearchPath file = case splitPath file of
    [_] -> do
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
    _ -> do
        exists <- doesFileExist file
        if exists
            then return (Right [file])
            else return (Left file)

main = do
    args <- parseArgs
    let fortunes = fortuneFiles args
    
    dist <- getDist args fortunes
    
    when (numEvents dist == 0) $ do
        hPutStrLn stderr "No fortunes matched the filter criteria"
        exitWith (ExitFailure 2)
    
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

-- longest one is long
checkThreshold t Long  s =      overThreshold t (maxChars s) (maxLines s)
-- shortest one is not long
checkThreshold t Short s = not (overThreshold t (minChars s) (minLines s))

overThreshold (Chars n) c l = c > n
overThreshold (Lines n) c l = l > n

getDist :: Args -> [FortuneFile] -> IO (Categorical Float (FortuneFile, Int, RVar Int))
getDist args files = equalize <$> case fortuneFilters args of
    [] -> do
        dist <- defaultFortuneDistribution files
        let f file = do
                n <- getNumFortunes file
                return (file, n, uniform 0 (n-1))
        T.mapM f dist
    filters -> do
        dist <- fortuneDistributionWhere (\f i e -> andM [p f i e | p <- filters]) files
        let f (file, iDist) = (file, numEvents iDist, rvar iDist)
        return (fmap f dist)
    where 
        equalize
            | equalProb args = mapCategoricalPs (const 1)
            | otherwise      = id
