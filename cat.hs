module Main where

-- cat command
-- reads files from cmdline or takes input from stdin

import Control.Monad (when)
import Control.Exception (IOException, try)
import Data.Either (isLeft)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

type Argument = String
type FileContent = String

collect :: [Argument] -> IO [Either IOException FileContent]
collect = mapM $ try . readFile

display :: [Either IOException FileContent] -> IO()
display []    = getContents >>= putStr
display files = do
        mapM toConsole files
        when (any isLeft files) exitFailure
    where
        toConsole (Left exception) = hPutStrLn stderr $ show exception
        toConsole (Right content)  = putStrLn content
        
main :: IO()
main = getArgs >>= collect >>= display