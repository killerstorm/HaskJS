{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
import Haste.Parsing
--import Haste.Serialize 
import Data.Either
import Control.Applicative

import qualified Data.Map as Map

import Debug.Trace

import CoinKernel
import TransactionGraph

txInputs       = toJSStr "txInputs"
txPayload      = toJSStr "txPayload"
txID           = toJSStr "txID"
txOutputCount  = toJSStr "txOutputCount"
hashHex        = toJSStr "hashHex"
index          = toJSStr "index"


coinstateMap = Map.fromList [(("2", 1), JustCS 1), (("3", 6), NullCS)]

apply' = (applyTx (toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel trivialCoinKernel))]))))


--packToJS :: Map.Map CoinId (WrappedCS cs) -> [JSString]
packToJS m = Prelude.foldr f [] $ Map.toList m
  where f x acc = (: acc) $ toJSStr $
                  "{" ++ "\"hashHex\""    ++ ":" ++ "\"" ++ a ++ "\"" ++ "," ++
                         "\"index\""      ++ ":" ++ b ++ "," ++
                         "\"coinState\""  ++ ":" ++ c ++ "}"              
                  where 
                        a = fst $ fst x
                        b = show . snd $ fst x
                        c = show $ snd x 
 
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
                (jsonToStr x hashHex, (\x -> read x :: Int) $ jsonToStr x index)
      _      -> acc


runCoinKernelOnGraph :: [JSString] -> IO [JSString]
runCoinKernelOnGraph xs = return . packToJS $ foldTxGraph g apply'
  where g = Prelude.foldl (\acc x -> parseToTx x : acc) [] xs
                        
getMuxShape :: JSString -> IO JSString
getMuxShape s = return s

        
main = do
  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
  export (toJSStr "getMuxShape") getMuxShape

