import System.Environment



dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "list" = list

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
