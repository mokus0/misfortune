module Bash (Quote(..), quote, main) where

import Control.Applicative hiding (Alternative(..))
import Control.Monad
import Data.Char
import Data.Fortune
import Text.HTML.TagSoup
import Text.Parsec
import qualified Data.Text as T
import Network.HTTP
import System.Environment
import System.FilePath
import System.IO

fetch url = simpleHTTP (getRequest url) >>= getResponseBody

trim p = f . f where f = reverse . dropWhile p
trimSpace = trim isSpace
mapText f = map f'
    where
        f' (TagText t) = TagText (f t)
        f' x = x
unBr = map f
    where
        f t | isTagOpenName "br" t  = TagText "\n"
            | isTagOpenName  "p" t  = TagText "\n"
            | isTagCloseName "p" t  = TagText "\n"
            | otherwise             = t

textify = trimSpace . innerText . unBr . mapText trimSpace

row n str = (partitions (isTagOpenName "tr") str !! n)

data Quote = Quote
    { qNum      :: !Int
    , qScore    :: !Int
    , qText     :: !String
    } deriving (Eq, Ord, Read, Show)

quote n = do
    let url = "http://bash.org/?quote=" ++ show (n :: Int)
    str <- fetch url
    let txt = textify (row 2 (parseTags str))
        Right it = parse p url txt
    return it
        where
            p = try (Right <$> ok) <|> Left <$> err
            int = read <$> many1 (satisfy isDigit)
            err = getInput
            ok = Quote <$> (char '#'           *> int)
                       <*> (string "+("        *> int)
                       <*> (string ")-[X]\n\n" *> getInput)

main = do
    home <- getEnv "HOME"
    f    <- openFortuneFile (home </> ".misfortune" </> "bash") '%' True
    let out str = putStrLn str >> appendFortune f (T.pack str)
    mapM_ (either (hPutStrLn stderr) (out . qText) <=< quote) =<< map read <$> getArgs
