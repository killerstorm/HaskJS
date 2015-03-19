{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
--import Haste.Parsing
--import Haste.Serialize
import Data.Either
import Control.Applicative
import System.Random as SR


import qualified Data.Map as Map

import Debug.Trace

import CoinKernel
import TransactionGraph

instance Pack JSON
instance Unpack JSON
instance Pack Integer
instance Unpack Integer
instance Pack (WrappedCS Integer)
instance Unpack (WrappedCS Integer)


pick :: [a] -> a
pick xs = (xs !!) $  fst $ Haste.randomR (0, length xs - 1) (mkSeed 5)


apply' = (applyTx (toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK)),
                                                 (1, (strictCoinKernel issueCK))    ]))))

kernel = toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK)),
                                                 (1, (strictCoinKernel issueCK))    ]))

packToString m = Prelude.foldr f [] $ Map.toList m
  where f x acc = (: acc) $ 
                  "{" ++ "\"txID\""       ++ ":" ++ "\"" ++ a ++ "\"" ++ "," ++
                         "\"index\""      ++ ":" ++ b ++ "," ++
                         "\"coinState\""  ++ ":" ++ "\"" ++ c ++ "\"" ++ "}"              
                  where 
                        a = fst $ fst x
                        b = show . snd $ fst x
                        c = show $ snd x 

--runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [String]
runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [(CoinId, Int)]
runCoinKernelOnGraph xs = return $ map (\(a, b) -> case b of
                                           JustCS x  -> (a, read (show x) :: Int)
                                           _         -> (a, 0)) $
                          Map.toList $ foldTxGraph g apply'
  where g = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
                        
getMuxShape :: String -> IO String
getMuxShape payload = return $ ms
  where ms = case parseMuxShape payload of
          Just x -> show $ fst x
          _      -> "Nothing"



main = do
  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
--  export (toJSStr "runCoinKernel") runCoinKernel
  export (toJSStr "getMuxShape") getMuxShape
