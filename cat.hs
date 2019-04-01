module Main where

-- cat command
-- reads files from cmdline or takes input from stdin
import qualified Data.ByteString.Char8 as L
import Control.Monad (when)
import Control.Exception (IOException, try)
import Data.Either (isLeft)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

type Argument = String
type FileContent = L.ByteString

collect :: [Argument] -> IO [Either IOException FileContent]
collect = mapM $ try . L.readFile

display :: [Either IOException FileContent] -> IO()
display []    = L.getContents >>= L.putStr
display files = do
        let hasFailure = any isLeft files
        mapM toConsole files
        when hasFailure exitFailure
    where
        toConsole (Left exception) = hPutStrLn stderr $ show exception
        toConsole (Right content)  = L.putStrLn content
        
main :: IO()
main = getArgs >>= collect >>= display