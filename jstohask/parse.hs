import Haste.Foreign
import Haste.Prim 
import Haste.JSON
import Haste.Parsing
import Haste.Serialize
import Data.Either


-- parse string to JSON then
-- pack it's part to JS object and send back
parse :: JSString -> IO JSAny
parse ob = return $ toObject b
  where Right x = decodeJSON ob
        a =  x ! (toJSStr "ins")  
        b =  x ! (toJSStr "outs")

main = do
  export (toJSStr "parse") parse
