{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON
import Haste.Parsing
import Haste.Serialize 
import Data.Either
import Control.Applicative

import Data.Map as Map

import CoinKernel
import TransactionGraph

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

instance Pack (Tx a)
instance Unpack (Tx a)

instance Serialize (Tx a) where

--  toJSON (Tx a)
--  parseJSON j =
    

runParser' :: (a -> Parser b) -> a -> Either String b
runParser' p x = case p x of Parser y -> y



        
main = do
  export (toJSStr "runCoinKernel") runCoinKernel
  export (toJSStr "getMuxShape") getMuxShape

