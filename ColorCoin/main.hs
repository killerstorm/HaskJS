{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
--import Haste.Parsing
--import Haste.Serialize
import Data.Either
import Control.Applicative

import qualified Data.Map as Map

import Debug.Trace

import CoinKernel
import TransactionGraph

instance Pack JSON
instance Unpack JSON

apply' = (applyTx (toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK))]))))

kernel = toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK))]))

packToString m = Prelude.foldr f [] $ Map.toList m
  where f x acc = (: acc) $ 
                  "{" ++ "\"txID\""       ++ ":" ++ "\"" ++ a ++ "\"" ++ "," ++
                         "\"index\""      ++ ":" ++ b ++ "," ++
                         "\"coinState\""  ++ ":" ++ "\"" ++ c ++ "\"" ++ "}"              
                  where 
                        a = fst $ fst x
                        b = show . snd $ fst x
                        c = show $ snd x 

runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [String]
runCoinKernelOnGraph xs = return . packToString $ foldTxGraph g apply'
  where g = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
                        
getMuxShape :: String -> IO String
getMuxShape payload = return $ ms
  where ms = case parseMuxShape payload of
          Just x -> show $ fst x
          _      -> "Nothing"

main = do
  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
  export (toJSStr "getMuxShape") getMuxShape
