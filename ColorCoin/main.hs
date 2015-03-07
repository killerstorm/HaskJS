{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
import Haste.Parsing
import Haste.Serialize
import Data.Either
import Control.Applicative

import qualified Data.Map as Map

import Debug.Trace

import CoinKernel
import TransactionGraph

instance Pack JSON
instance Unpack JSON


instance Pack (Tx String)
instance Unpack (Tx String)

txInputs       = toJSStr "txInputs"
txPayload      = toJSStr "txPayload"
txID           = toJSStr "txID"
txOutputCount  = toJSStr "txOutputCount"
hashHex        = toJSStr "hashHex"
index          = toJSStr "index"


{-
instance Serialize (Tx String) where
  toJSON (Tx a b c d) = toJSON $ toJSStr "test"

  parseJSON j =
    Tx <$>
        j .: txPayload
    <*> j .: txInputs
    <*> j .: txOutputCount
    <*> j .: txID
-}


coinstateMap = Map.fromList [(("2", 1), JustCS 1), (("3", 6), NullCS)]

apply' = (applyTx (toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel trivialCoinKernel))]))))

kernel = toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel trivialCoinKernel))]))

packToString m = Prelude.foldr f [] $ Map.toList m
  where f x acc = (: acc) $ 
                  "{" ++ "\"hashHex\""    ++ ":" ++ "\"" ++ a ++ "\"" ++ "," ++
                         "\"index\""      ++ ":" ++ b ++ "," ++
                         "\"coinState\""  ++ ":" ++ c ++ "}"              
                  where 
                        a = fst $ fst x
                        b = show . snd $ fst x
                        c = show $ snd x 
{-- 
jsonToStr :: JSON -> JSString -> String
jsonToStr j s = fromJSStr . encodeJSON $ (J.!) j s

parseToTx :: JSON -> Tx String
parseToTx json = trace ("payload = " ++ show c) $ Tx a b c d
  where 
        a  = tail . init $ jsonToStr json txPayload
        b  = getInputs (json J.! txInputs) 0 []
        c  = jsonToStr json txID
        d  = (\x -> read x :: Int) $ jsonToStr json txOutputCount


getInputs :: JSON -> Int -> [CoinId] -> [CoinId]
getInputs j c acc = 
    case j J.~> toJSStr (show c) of
      Just x -> getInputs j (c + 1) $ (: acc)
                (jsonToStr x hashHex, (\x -> read x :: Int) $ jsonToStr x index)
      _      -> acc
--}

runCoinKernelOnGraph :: [(String, [(String, Int)], String, Int)] -> IO [String]
runCoinKernelOnGraph xs = return . packToString $ foldTxGraph (topologicalSort g g) apply'
  where g = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
                        
getMuxShape :: String -> IO String
getMuxShape payload = return $ ms
  where ms = case parseMuxShape payload of
          Just x -> show $ fst x
          _      -> "Nothing"

test :: [(String, [(String, Int)], String, Int)] -> IO JSString
test transactions = return $ toJSStr $ payload $ head tx
    where tx = map (\(a, b, c, d) -> Tx a b c d) transactions



main = do
  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
  export (toJSStr "getMuxShape") getMuxShape
  export (toJSStr "test") test
