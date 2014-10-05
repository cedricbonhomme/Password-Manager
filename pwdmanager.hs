import System.Environment
import Data.Aeson             ((.:), (.:?), decode, FromJSON(..), Value(..))



dispatch :: String -> [String] -> IO ()
dispatch "add" = add

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName , service, userName, password] =
    appendFile fileName (service ++ ":" ++ userName ++ ":" ++ password ++ "\n")
