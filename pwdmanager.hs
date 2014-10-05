import System.Environment



dispatch :: String -> [String] -> IO ()
dispatch "add" = add

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName , service, userName, password] =
    appendFile fileName (service ++ ":" ++ userName ++ ":" ++ password ++ "\n")
