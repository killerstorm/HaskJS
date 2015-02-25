{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
import Haste.Parsing
import Haste.Serialize 
import Data.Either
import Control.Applicative

import Data.Map as Map

import CoinKernel
import TransactionGraph

ins       = toJSStr "ins"
pload     = toJSStr "pload"
txid      = toJSStr "txid"
outs      = toJSStr "outs"
hashHex   = toJSStr "hashHex"
index     = toJSStr "index"
ob        = "{"
cb        = "}"
cn        = ":"
cm        = ","

{--
instance Serialize (Tx String) where
  
  toJSON j = toJSON $ toJSStr $
     ob  ++ (Prelude.foldr (\x acc -> fst x ++ acc) "" (inputs j)) ++ cb

  parseJSON j = do
     a <- j .: pload
     b <- j .: ins
     c <- j .: txid
     d <- j .: outcount
     return $ Tx a b c d


runParser' :: (a -> Parser b) -> a -> Either String b
runParser' p x = case p x of Parser y -> y
--}

jsonToStr :: JSON -> JSString -> String
jsonToStr j s = fromJSStr . encodeJSON $ (J.!) j s

parseToTx :: JSString -> Tx String
parseToTx s = Tx a b c d
  where Right json = decodeJSON s
        a  = jsonToStr json pload
        b  = getInputs (json J.! ins) 0 []
        c  = jsonToStr json txid
        d  = (\x -> read x :: Int) $ jsonToStr json outs

getInputs :: JSON -> Int -> [CoinId] -> [CoinId]
getInputs j c acc = 
    case j J.~> toJSStr (show c) of
      Just x -> getInputs j (c + 1) $ (: acc)
                (jsonToStr x hashHex, (\x -> read x :: Int) $ jsonToStr x  index)
      _      -> acc


runCoinKernel :: [JSString] -> -- ?????????????????
runCoinKernel s =  return $    -- ????????????????

                        
getMuxShape :: JSString -> -- ??????????????
getMuxShape s = return     -- ??????????????

        
main = do
  export (toJSStr "runCoinKernel") runCoinKernel
  export (toJSStr "getMuxShape") getMuxShape

