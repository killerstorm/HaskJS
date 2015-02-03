import Haste.Foreign
import Haste.Prim 
import Haste.JSON
import Data.Either

-- parse string to JSON then
-- pack it to JS object and send back
parse :: String -> IO JSAny
parse ob = return $ toObject x
  where Right x = decodeJSON $ toJSStr ob
       

main = do
  export (toJSStr "parse") parse
