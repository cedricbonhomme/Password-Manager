import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception



dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "list" = list
dispatch "remove" = remove

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName , service, userName, password] =
    appendFile fileName (service ++ ":" ++ userName ++ ":" ++ password ++ "\n")

list :: [String] -> IO ()
list [fileName] = do
    contents <- readFile fileName
    let entries = lines contents
        numberedEntries = zipWith (\n line -> show n ++ " - "  ++ line)
                            [0..] entries
    putStr $ unlines numberedEntries

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let entries = lines contents
        number = read numberString
        newEntries = unlines $ delete (entries !! number) entries
    replaceFileContents fileName newEntries

replaceFileContents :: FilePath -> String -> IO ()
replaceFileContents fileName contents =
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle contents
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)