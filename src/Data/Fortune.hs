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
     , listFortuneFilesIn
     
     , findFortuneFile
     , findFortuneFileIn
     , findFortuneFilesIn
     
     , FortuneType(..)
     , getFortuneDir
     , defaultFortuneFiles
     
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
import Control.Monad
import Data.Char
import Data.Function
import Data.Maybe
import Data.Monoid (First(..))
import Data.Random hiding (Normal)
import Data.Random.Distribution.Categorical
import Data.Semigroup hiding (All, First(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Paths_misfortune
import System.Directory
import System.Environment
import System.FilePath
import System.Random.Stateful (newIOGenM, newStdGen)

-- |The number of fortune strings in the index
numFortunes :: S.FortuneStats -> Int
numFortunes = getSum . S.numFortunes

-- |The smallest number of characters in any string in the index
minChars :: S.FortuneStats -> Int
minChars    = getMin . S.minChars

-- |The greatest number of characters in any string in the index
maxLines :: S.FortuneStats -> Int
maxLines    = getMax . S.maxLines

-- |The smallest number of lines in any string in the index
minLines :: S.FortuneStats -> Int
minLines    = getMin . S.minLines

-- |The greatest number of lines in any string in the index
maxChars :: S.FortuneStats -> Int
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

isIndexPath path = case takeExtension path of
    ".ix"   -> True
    ".dat"  -> True
    _       -> False

-- |List all the fortune files in a directory.  The 'Bool' value
-- specifies whether to search subtrees as well.
--
-- Any file which does not have an extension of \".ix\" or \".dat\"
-- will be reported as a fortune file (\".dat\" is not used by
-- misfortune, but is ignored so that misfortune can share fortune
-- databases with @fortune@).
listFortuneFiles :: Bool -> FilePath -> IO [FilePath]
listFortuneFiles rec = traverseDir rec onFile
    where onFile path = return [ path | not (isIndexPath path) ]

-- |List all the fortune files in several directories.  Each directory
-- will be searched by 'listFortuneFiles' (using the corresponding 'Bool' 
-- value to control whether the directory is searched recursively) and all
-- results will be combined.
listFortuneFilesIn :: [(FilePath, Bool)] -> IO [FilePath]
listFortuneFilesIn = fmap concat . mapM (uncurry (flip listFortuneFiles))

-- |Like 'listFortuneFiles' except only returning paths with the 
-- specified file name.
findFortuneFile :: Bool -> FilePath -> String -> IO [FilePath]
findFortuneFile rec dir file = search dir
    where 
        search = traverseDir rec onFile
        onFile path = return [ path | takeFileName path == file ]

-- |Like 'listFortuneFilesIn' except only returning paths with the 
-- specified file name.
findFortuneFileIn :: [(String, Bool)] -> String -> IO [FilePath]
findFortuneFileIn dirs file = concat <$> sequence
    [ findFortuneFile rec dir file | (dir, rec) <- dirs]

-- |Like 'findFortuneFileIn' but searches for multiple files in multiple directories.
findFortuneFilesIn :: [(String, Bool)] -> [String] -> IO [FilePath]
findFortuneFilesIn dirs files = 
    concat <$> mapM (findFortuneFileIn dirs) files

-- |Three different search paths are supported, depending on the \"type\" of fortune
-- requested.  These are the types that can be requested.
data FortuneType
    = All
    | Normal
    | Offensive
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- |Get the path of the directory containing built-in fortunes of the specified type.
getFortuneDir :: FortuneType -> IO FilePath
getFortuneDir fortuneType = do
    dir <- getDataDir
    return $! case fortuneType of
        All         -> dir </> "data"
        Normal      -> dir </> "data" </> "normal"
        Offensive   -> dir </> "data" </> "offensive"

-- |Get a list of all fortune files on the configured search path (see 'getFortuneSearchPath')
defaultFortuneFiles :: FortuneType -> IO [FilePath]
defaultFortuneFiles fortuneType = 
    getFortuneSearchPath fortuneType >>= listFortuneFilesIn

-- |Get the default search path for a specified fortune type (ignoring the @MISFORTUNE_PATH@ environment variables)
defaultFortuneSearchPath :: FortuneType -> IO [(FilePath, Bool)]
defaultFortuneSearchPath fortuneType = do
    dir <- getFortuneDir fortuneType
    return [(dir, True)]

getEnv' typeStr key = do
    env <- getEnvironment
    let lookup' k = First . lookup k
    return $ getFirst (lookup' (key ++ "_" ++ typeStr) env
                    <> lookup' key env)

-- |Get the configured search path for a specified fortune type.
-- If the environment variable @MISFORTUNE_PATH_<TYPE>@ is set, it will be used.
-- Otherwise, if @MISFORTUNE_PATH@ is set, it will be used.  Otherwise, the
-- 'defaultFortuneSearchPath' will be used.
--
-- Environment variables are interpreted by splitting on @':'@ and checking
-- for an optional '+' or '-' prefix on each component (where '+' indicates 
-- recursive search of that directory).  The default is non-recursive search
-- for each component.
getFortuneSearchPath :: FortuneType -> IO [(FilePath, Bool)]
getFortuneSearchPath defaultType
    = getEnv' (map toUpper $ show defaultType) "MISFORTUNE_PATH"
    >>= maybe (defaultFortuneSearchPath defaultType)
              (return . map f . split)
    >>= filterM (doesDirectoryExist . fst)
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

-- |Search for all fortune files in the configured search path with the given name.
resolveFortuneFile :: FortuneType -> String -> IO [FilePath]
resolveFortuneFile defaultType file = do
    dirs <- getFortuneSearchPath defaultType
    findFortuneFileIn dirs file

-- |Search for all fortune files in the configured search path with any of the given names.
resolveFortuneFiles :: FortuneType -> [String] -> IO [FilePath]
resolveFortuneFiles defaultType files = do
    dirs <- getFortuneSearchPath defaultType
    findFortuneFilesIn dirs files

-- |Select a random fortune from all files matching any of a list of names (or if the 
-- list is empty, all fortune files on the search path).  Every fortune string will have
-- an equal probability of being selected.
randomFortune :: [String] -> IO String
randomFortune [] = do
    paths <- defaultFortuneFiles Normal
    if null paths
        then return "Very few profundities can be expressed in less than 80 characters."
        else randomFortune paths

randomFortune paths = withFortuneFiles '%' False paths $ \fs -> do
    randomFortuneFromRandomFile . rvar =<< defaultFortuneDistribution fs

-- |Select a random fortune file from a specified distribution and then select a
-- random fortune from that file (unformly).
randomFortuneFromRandomFile :: RVar FortuneFile -> IO String
randomFortuneFromRandomFile file = do
    gen <- newStdGen >>= newIOGenM
    f <- sampleFrom gen file
    n <- getNumFortunes f
    i <- sampleFrom gen (uniform 0 (n-1))
    T.unpack <$> getFortune f i

-- |Given a list of 'FortuneFile's, compute a distrubution over them weighted by the number
-- of fortunes in each.  If this distribution is used with 'randomFortuneFromRandomFile',
-- the result will be a uniform selection over all the fortunes in all the files.
defaultFortuneDistribution :: [FortuneFile] -> IO (Categorical Float FortuneFile)
defaultFortuneDistribution [] = fail "defaultFortuneDistribution: no fortune files"
defaultFortuneDistribution fs = fromWeightedList <$> sequence
    [ do
        weight <- getNumFortunes f
        return (fromIntegral weight, f)
    | f <- fs
    ]

-- |Like 'defaultFortuneDistribution', but filtering the fortunes.  In addition to the
-- fortune file, the tuples in the distribution include a distribution over the
-- matching fortune indices in that file, assigning equal weight to each.
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

-- |Perform an action with an open 'FortuneFile', ensuring the file is closed
-- when the action finishes.
withFortuneFile :: Char -> Bool -> FilePath -> (FortuneFile -> IO a) -> IO a
withFortuneFile delim writeMode path = 
    bracket (openFortuneFile delim writeMode path)
             closeFortuneFile

-- |Perform an action with many open 'FortuneFile's, ensuring the files are closed
-- when the action finishes.
withFortuneFiles :: Char -> Bool -> [FilePath] -> ([FortuneFile] -> IO a) -> IO a
withFortuneFiles _ _ [] action = action []
withFortuneFiles delim writeMode (p:ps) action =
    withFortuneFile delim writeMode p $ \p ->
        withFortuneFiles delim writeMode ps $ \ps ->
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
