{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Main where

import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
import Haste.Serialize
import Data.Either
import Control.Applicative
import System.Random as SR

import qualified Data.Map as Map

import Debug.Trace

import Src.CoinKernel
import Src.TransactionGraph
import Src.Types

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


packToJSON :: (Show cs) => (CoinId, WrappedCS cs, Integer) -> JSString
packToJSON ((txid, index), cs, value) = encodeJSON $ toJSON $ Coin txid index value coinstate
  where coinstate = case cs of
          JustCS x  -> show x
          MissingCS -> "M"
          InvalidCS -> "I"
          NullCS    -> "N"


topSort :: [(String, [CoinId], TxId, Int)] -> IO [(String, [CoinId], TxId, Int)]
topSort xs = return $  map (\(Tx a b c d) -> (a, b, c, d)) g
  where g' = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
        g  = reverse $ topologicalSort g' g' -- full sorted graph

             
runKernel :: JSString -> IO [JSString]
runKernel txJSON = return $ map packToJSON coins
  where
    tx       = decodeTxJSON txJSON
    outputs  = kernel (payload tx) []
    outs :: [Integer]
    outs     =  read (snd . head $
                     (reads (snd . head $
                     (reads (payload tx) :: [(([Int], [Int], Int), String)])) :: [(Int, String)] )) :: [Integer]
    coins    = zip3 (zip (repeat (txId tx)) [0..]) outputs outs


decodeTxJSON txJSON = let
                       tx :: (Serialize a) =>  Either String (Tx a)
                       tx = do
                         json <- decodeJSON txJSON
                         fromJSON json
                      in case tx of
                        Left s   -> error s
                        Right j  -> j

getMuxShape :: String -> IO String
getMuxShape payload = return $ ms
  where ms = case parseMuxShape payload of
          Just x -> show $ fst x
          _      -> "Nothing"
{-
runCoinKernelOnGraph :: [JSString] -> JSString -> IO [JSString]
runCoinKernelOnGraph xs = return $ map packToJSON $
                          Map.toList $ foldTxGraph g apply'
  where g' = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
        g  = reverse $ topologicalSort g' g'-- full sorted graph
-}
{-
--runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [String]
_runCoinKernelOnGraph :: (String, [CoinId], TxId, Int) ->  [(String, [CoinId], TxId, Int)] -> IO [JSString]
_runCoinKernelOnGraph tx xs = return $ map packToJSON $
                          Map.toList $ foldTxGraph g apply'
  where g' =  Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
        t  = getTx' tx
        g  = reverse $ topologicalSort g' [t] -- full sorted graph
-}


{--
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
-}

main = do
--  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
--  export (toJSStr "_runCoinKernelOnGraph") _runCoinKernelOnGraph
--  export (toJSStr "runCoinKernelOn") runCoinKernelOn
  export (toJSStr "topSort") topSort
  export (toJSStr "runKernel") runKernel
  export (toJSStr "getMuxShape") getMuxShape
--  export (toJSStr "toposort") toposort

