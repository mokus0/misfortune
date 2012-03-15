module Split where

import Control.Applicative
import Data.Fortune
import qualified Data.Text as T
import System.Exit
import System.IO

-- quick and dirty review system to scan through a fortune file and let 
-- me separate potentially-offensive ones from the rest

main = do
    f <- openFortuneFile "foo" '%' False
    
    normal <- openFile "normal" AppendMode
    off    <- openFile "off" AppendMode
    
    n <- getNumFortunes f
    hSetBuffering stdin NoBuffering
    mapM_ (choose f normal off) [0..n-1]

choose f normal off i = do
    fortune <- T.unpack <$> getFortune f i
    
    putStrLn "========================================================"
    putStrLn fortune
    putStrLn "========================================================"
    
    let query = do
            putStr "\na/o/s/q"
            hFlush stdout
            
            x <- getChar
            putStrLn ""
            
            case x of
                'a' -> hPutStr normal (fortune ++ "\n%\n") >> hFlush normal
                'o' -> hPutStr off    (fortune ++ "\n%\n") >> hFlush off
                's' -> putStrLn " ...skipping..."
                'q' -> exitWith ExitSuccess
                _   -> query
    query
