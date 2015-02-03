import Haste.Foreign
import Haste.Prim 
import Haste.JSON
import Haste.Parsing
import Haste.Serialize
import Data.Either

ins      = toJSStr "ins"
outs     = toJSStr "outs"
hash     = toJSStr "hash"
index    = toJSStr "index"
script   = toJSStr "script"
buffer   = toJSStr "buffer"
chunks   = toJSStr "chunks"
seq      = toJSStr "sequence"
value    = toJSStr "value"
version  = toJSStr "version"
locktime = toJSStr "locktime"

-- parse string to JSON then
-- pack it's part to JS object and send back
parse :: JSString -> IO JSAny
parse ob = return $ toObject a
  where Right x = decodeJSON ob
        a =  x ! ins  
        b =  x ! outs

main = do
  export (toJSStr "parse") parse
