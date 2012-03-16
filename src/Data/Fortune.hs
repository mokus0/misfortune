module Data.Fortune
     ( module Data.Fortune.FortuneFile
     , module Data.Fortune.Index
     
     , S.FortuneStats
     , numFortunes
     , minChars
     , maxLines
     , minLines
     , maxChars
     
     , listFortuneFiles
     , findFortuneFile
     , findFortuneFileIn
     , findFortuneFilesIn
     
     , FortuneType(..)
     , getFortuneDir
     , defaultFortuneFiles
     , defaultFortuneFileNames
     
     , defaultFortuneSearchPath
     , getFortuneSearchPath
     , resolveFortuneFile
     , resolveFortuneFiles
     
     , randomFortune
     , randomFortuneFromRandomFile
     , defaultFortuneDistribution
     , fortuneDistributionWhere
     
     , withFortuneFile
     , withFortuneFiles
     
     , mapFortunesWithIndexM
     , mapFortunesWithIndex
     , mapFortunesM
     , mapFortunes
     
     , filterFortunesWithIndexM
     , filterFortunesWithIndex
     , filterFortunesM
     , filterFortunes
     ) where

import Data.Fortune.FortuneFile
import Data.Fortune.Index
import qualified Data.Fortune.Stats as S

import Control.Applicative
import Control.Exception
import Data.Function
import Data.Maybe
import Data.Random hiding (Normal)
import Data.Random.Distribution.Categorical
import Data.Semigroup hiding (All)
import qualified Data.Text as T
import qualified Data.Vector as V
import Paths_misfortune
import System.Directory
import System.Environment
import System.FilePath

numFortunes = getSum . S.numFortunes
minChars    = getMin . S.minChars
maxLines    = getMax . S.maxLines
minLines    = getMin . S.minLines
maxChars    = getMax . S.maxChars


-- list the full paths of all visible items in the given directory
listDir dir =
    map (dir </>) . filter (not . hidden) <$> getDirectoryContents dir
    where hidden name = take 1 name == "."

traverseDir rec onFile = fix $ \search dir ->
    let onItem path = do
            isDir <- doesDirectoryExist path
            if isDir
                then if rec 
                    then search path
                    else return []
                else onFile path
     in concat <$> (mapM onItem =<< listDir dir)

doesFortuneFileExist path = liftA2 (&&)
    (doesFileExist path)
    (doesIndexFileExist path)

doesIndexFileExist path = doesFileExist (path <.> "dat")

listFortuneFiles rec = traverseDir rec onFile
    where onFile path
            | takeExtension path == ".dat"  = return []
            | otherwise = do
                hasIndex <- doesIndexFileExist path
                return [path | hasIndex ]

findFortuneFile rec dir file = liftA2 (++) checkHere (search dir)
    where 
        checkHere
            | dir /= "."    = return []
            | otherwise     = case splitPath file of
                [_] -> return [] -- search will find it
                _   -> do
                    exists <- doesFortuneFileExist file
                    return [file | exists]
        
        search = traverseDir rec onFile
        onFile path 
            | takeFileName path /= file = return []
            | otherwise = do
                hasIndex <- doesIndexFileExist path
                return [ path | hasIndex ]

findFortuneFileIn dirs file = concat <$> sequence
    [ findFortuneFile rec dir file | (dir, rec) <- dirs]

findFortuneFilesIn dirs files = 
    concat <$> mapM (findFortuneFileIn dirs) files

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
    getFortuneDir fortuneType >>= listFortuneFiles True

defaultFortuneFileNames fType = map takeFileName <$> defaultFortuneFiles fType

defaultFortuneSearchPath fortuneType = do
    dir <- getFortuneDir fortuneType
    return [(".", False), (dir, True)]

getEnv' key = lookup key <$> getEnvironment
getFortuneSearchPath defaultType
    = getEnv' "MISFORTUNE_PATH"
    >>= maybe (defaultFortuneSearchPath defaultType)
              (return . map f . split)
    where
        -- entries with a '+' will be searched recursively
        -- paths that actually start with a '+', such as "+foo",
        -- can be given as '++foo' or '-+foo'
        f ('+' : it) = (it, True)
        f ('-' : it) = (it, False)
        f it         = (it, False)
        
        split [] = []
        split xs = a : split (drop 1 b)
            where (a, b) = break (':' ==) xs

resolveFortuneFile defaultType file = do
    dirs <- getFortuneSearchPath defaultType
    findFortuneFileIn dirs file

resolveFortuneFiles defaultType files = do
    dirs <- getFortuneSearchPath defaultType
    findFortuneFilesIn dirs files

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

-- |Events are (fortune file, distribution over matching fortune indices)
fortuneDistributionWhere 
    :: (FortuneFile -> Int -> IndexEntry -> IO Bool) 
    -> [FortuneFile]
    -> IO (Categorical Float (FortuneFile, Categorical Float Int))
fortuneDistributionWhere p files =
    fromWeightedList <$> sequence
        [ do
            is <- filterFortunesWithIndexM (p f) f
            let iDist = fromObservations is
            return (fromIntegral (numEvents iDist), (f, iDist))
        | f <- files
        ]
withFortuneFile path delim writeMode = 
    bracket (openFortuneFile path delim writeMode)
             closeFortuneFile

withFortuneFiles [] _ _ action = action []
withFortuneFiles (p:ps) delim writeMode action =
    withFortuneFile  p  delim writeMode $ \p ->
        withFortuneFiles ps delim writeMode $ \ps ->
            action (p:ps)

mapFortunesWithIndexM p f = 
    mapM (uncurry p) . zip [0..] . V.toList =<< getEntries =<< getIndex f

mapFortunesWithIndex p = mapFortunesWithIndexM (return . p)

mapFortunesM p = mapFortunesWithIndexM (const p)
mapFortunes  p = mapFortunesM (return . p)

filterFortunesWithIndexM p = fmap catMaybes . mapFortunesWithIndexM p'
    where
        p' i e = fmap (toMaybe i) (p i e)
        toMaybe i True  = Just i
        toMaybe _ False = Nothing

filterFortunesWithIndex p = filterFortunesWithIndexM (\i e -> return $! p i e)

filterFortunesM p = filterFortunesWithIndexM (const p)
filterFortunes  p = filterFortunesWithIndex  (const p)
