
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
import qualified Haste.Serialize as S
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

apply' = (applyTx (toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK)),
                                                 (1, (strictCoinKernel issueCK))    ]))))


kernel = toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK)),
                                                 (1, (strictCoinKernel issueCK))    ]))

packToJSON :: (Show cs) => (CoinId, WrappedCS cs) -> JSString
packToJSON ((txid, index), cs) = toJSStr $
  "{" ++
  "\"txid\" : \""  ++ txid       ++ "\", " ++
  "\"index\" : " ++ show index ++ ", " ++
<<<<<<< HEAD
  "\"value\" : \""    ++ coinstate  ++ "\"}"
=======
  "\"coinstate\" : \""    ++ coinstate  ++ "\"}"
  where coinstate = case cs of
          JustCS x  -> show x
          MissingCS -> "M"
          InvalidCS -> "I"
          NullCS    -> "N"


packToJSON' :: (Show cs) => (CoinId, WrappedCS cs, Integer) -> JSString
packToJSON' ((txid, index), cs, value) = toJSStr $
  "{" ++
  "\"txid\" : \""  ++ txid       ++ "\", " ++
  "\"index\" : " ++ show index ++ ", " ++
  "\"coinstate\" : \""    ++ coinstate  ++ "\", " ++
  "\"value\" : " ++ show value ++ "}"
>>>>>>> develop
  where coinstate = case cs of
          JustCS x  -> show x
          MissingCS -> "M"
          InvalidCS -> "I"
          NullCS    -> "N"

--runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [String]
runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [JSString]
runCoinKernelOnGraph xs = return $ map packToJSON $
                          Map.toList $ foldTxGraph g apply'
  where g' = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
        g  = reverse $ topologicalSort g' g'-- full sorted graph

<<<<<<< HEAD
=======
--runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [String]
_runCoinKernelOnGraph :: (String, [CoinId], TxId, Int) ->  [(String, [CoinId], TxId, Int)] -> IO [JSString]
_runCoinKernelOnGraph tx xs = return $ map packToJSON $
                          Map.toList $ foldTxGraph g apply'
  where g' =  Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
        t  = getTx' tx
        g  = reverse $ topologicalSort g' [t] -- full sorted graph

getTx' :: (String, [CoinId], TxId, Int) -> Tx String 
getTx' (a, b, c, d) = (Tx a b c d)

>>>>>>> develop
topSort :: [(String, [CoinId], TxId, Int)] -> IO [(String, [CoinId], TxId, Int)]
topSort xs = return $  map (\(Tx a b c d) -> (a, b, c, d)) g
  where g' = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
        g  = reverse $ topologicalSort g' g' -- full sorted graph

             
<<<<<<< HEAD
runKernel :: (String, [(CoinId, Integer)], TxId, Int) -> IO [JSString]
runKernel (payload, inputs, txid, _) = return $ map packToJSON coins
  where outputs      = kernel payload $ map (JustCS . snd) inputs
        coins        = zip (zip (repeat txid) [0..]) outputs
=======
runKernel :: (String, TxId) ->  [(CoinId, Integer)] -> [Integer] -> IO [JSString]
runKernel (payload, txid) inputs  outs = return $ map packToJSON' coins
  where outputs      = kernel payload $ map (JustCS . snd) inputs
        coins        = zip3 (zip (repeat txid) [0..]) outputs outs
>>>>>>> develop
                       
                        
getMuxShape :: String -> IO String
getMuxShape payload = return $ ms
  where ms = case parseMuxShape payload of
          Just x -> show $ fst x
          _      -> "Nothing"


runCoinKernelOn :: [(String, ([(String, Int)], String, Int))] -> IO [(CoinId, Int)]
runCoinKernelOn xs = 
  return $ map (\(a, b) -> case b of
                   JustCS x  -> (a, read (show x) :: Int)
                   _         -> (a, 0)) $ Map.toList $ foldTxGraph g' apply'
  where
    (_, g) = topologicalSort' (Map.fromList xs) (map fst xs)
    g' = map getTx $ tail $ reverse g



toposort::  [(String, ([(String, Int)], String, Int))] -> IO [String]
toposort xs = return $ map show  g'
  where
    (_, g) = topologicalSort' (Map.fromList xs) (map fst xs)
    g'  = map getTx $ reverse  g
    f n = (fst n, snd n)
<<<<<<< HEAD


getTx :: (TxId, ([CoinId], String, Int)) -> Tx String 
getTx x =  (Tx c b a d)
    where
      a = fst x
      (b, c, d) = snd x

=======
>>>>>>> develop


getTx :: (TxId, ([CoinId], String, Int)) -> Tx String 
getTx x =  (Tx c b a d)
    where
      a = fst x
      (b, c, d) = snd x

main = do
  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
<<<<<<< HEAD
=======
  export (toJSStr "_runCoinKernelOnGraph") _runCoinKernelOnGraph
>>>>>>> develop
  export (toJSStr "runCoinKernelOn") runCoinKernelOn
  export (toJSStr "topSort") topSort
  export (toJSStr "runKernel") runKernel
  export (toJSStr "getMuxShape") getMuxShape
  export (toJSStr "toposort") toposort
