{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON
import Haste.Parsing
import Haste.Serialize 
import Data.Either
import Control.Applicative

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
nothing   = toJSStr "nothing"
ob        = "{"
cb        = "}"
cn        = ":"
cm        = ","

instance Pack JSON
instance Unpack JSON

data Transaction = Transaction {
  vers :: JSString,
  lock :: JSString,
  input :: JSString,
  output :: JSString
  } 

instance Serialize Transaction where
  
  toJSON (Transaction v l i o) = toJSON $ toJSStr $
     ob ++
     "version"  ++ cn ++ fromJSStr v ++ cm ++
     "locktime" ++ cn ++ fromJSStr l ++ cm ++
     "ins"      ++ cn ++ fromJSStr i ++ cm ++
     "outs"     ++ cn ++ fromJSStr o ++ cb

  parseJSON j =
    Transaction <$>
        j .: version
    <*> j .: locktime
    <*> j .: ins
    <*> j .: outs
    

runParser' :: (a -> Parser b) -> a -> Either String b
runParser' p x = case p x of Parser y -> y



-- parse string to JSON then
-- pack it's part to JS object and send back
                             
-- TODO: Find out correct way to decode/encode JSON
                             
parse :: JSString -> IO JSON
parse ob = return $ toJSON f
  where Right x = decodeJSON ob
        f :: Either String Transaction
        f = runParser' parseJSON x

     
main = do
  export (toJSStr "parse") parse
