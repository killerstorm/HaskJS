import Haste.Foreign
import Haste.Prim 
import Haste.JSON
import Haste.Parsing
import Haste.Serialize 
import Data.Either

ins       = toJSStr "ins"
outs      = toJSStr "outs"
hash      = toJSStr "hash"
index     = toJSStr "index"
script    = toJSStr "script"
buffer    = toJSStr "buffer"
chunks    = toJSStr "chunks"
sequence' = toJSStr "sequence"
value     = toJSStr "value"
version   = toJSStr "version"
locktime  = toJSStr "locktime"


--fromParser :: Parser JSON -> JSONfromParser a = a :: JSON
instance Pack JSON
instance Unpack JSON

-- parse string to JSON then
-- pack it's part to JS object and send back
parse :: JSString -> IO JSAny
parse ob = return $ toObject i
  where Right x = decodeJSON ob
        Dict dict = x
        Just i = lookup ins dict
        
main = do
  export (toJSStr "parse") parse
