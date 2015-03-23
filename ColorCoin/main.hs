{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
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

--runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [String]
runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [(CoinId, Int)]
runCoinKernelOnGraph xs = return $ map (\(a, b) -> case b of
                                           JustCS x  -> (a, read (show x) :: Int)
                                           _         -> (a, 0)) $
                          Map.toList $ foldTxGraph g apply'
  where g' = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
        g  = reverse $ topologicalSort g' g' -- full sorted graph
                 
runKernel :: (String, [(CoinId, Integer)], TxId) -> IO [(CoinId, Int)]
runKernel (payload, inputs, txid) = return coins
  where outputs      = kernel payload $ map (JustCS . snd) inputs
        coins        = zip (zip (repeat txid) [0..]) $ map (\(JustCS x) -> read (show x) :: Int) outputs
                       
                        
getMuxShape :: String -> IO String
getMuxShape payload = return $ ms
  where ms = case parseMuxShape payload of
          Just x -> show $ fst x
          _      -> "Nothing"



main = do
  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
  export (toJSStr "runKernel") runKernel
  export (toJSStr "getMuxShape") getMuxShape
