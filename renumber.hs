-- To turn a file list like: 01.first 02.second 03.third  into, say: 25.first 26.second 27.third

import System.Environment
import Data.List
import Text.Printf
import System.Posix.Files
import System.Directory

prependNewTag :: FilePath -> FilePath -> FilePath
prependNewTag orig newNum = newNum ++ ( dropWhile (/='.') orig )

myIntToTag :: Int -> FilePath
myIntToTag i = printf "%.4d" i

renameFiles :: [FilePath] -> Int -> [IO ()]
renameFiles [] startNum = []
renameFiles (x:xs) startNum = ( rename x ( prependNewTag x $ myIntToTag startNum ) ) : renameFiles xs (startNum + 1)

doFiles :: Int -> IO ()
doFiles startNum = do
    files <- fmap sort $ getDirectoryContents "."
    sequence_ $ renameFiles ( dropWhile ( (=='.') . head ) files ) startNum 

main = do
    args <- getArgs
    let num = (read::String -> Int) $ head args
    doFiles num
