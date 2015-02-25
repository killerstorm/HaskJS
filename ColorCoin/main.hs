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

txInputs       = toJSStr "txInputs"
txPayload      = toJSStr "txPayload"
txID           = toJSStr "txID"
txOutputCount  = toJSStr "txOutputCount"
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

apply' = (applyTx (toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel trivialCoinKernel))]))))


jsonToStr :: JSON -> JSString -> String
jsonToStr j s = fromJSStr . encodeJSON $ (J.!) j s

parseToTx :: JSString -> Tx String
parseToTx s = Tx a b c d
  where Right json = decodeJSON s
        a  = jsonToStr json txPayload
        b  = getInputs (json J.! txInputs) 0 []
        c  = jsonToStr json txID
        d  = (\x -> read x :: Int) $ jsonToStr json txOutputCount

getInputs :: JSON -> Int -> [CoinId] -> [CoinId]
getInputs j c acc = 
    case j J.~> toJSStr (show c) of
      Just x -> getInputs j (c + 1) $ (: acc)
                (jsonToStr x hashHex, (\x -> read x :: Int) $ jsonToStr x  index)
      _      -> acc


runCoinKernel :: [JSString] -> IO JSString
runCoinKernel xs = return $ toJSStr . show $ foldTxGraph g apply'
  where g = Prelude.foldl (\acc x -> parseToTx x : acc) [] xs
                        
getMuxShape :: JSString -> IO JSString
getMuxShape s = return s

        
main = do
  export (toJSStr "runCoinKernel") runCoinKernel
  export (toJSStr "getMuxShape") getMuxShape

