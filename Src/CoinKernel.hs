{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Src.CoinKernel where


import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
import Haste.Serialize

import Debug.Trace
import Data.Maybe
import qualified Data.Map as Map
import Src.Types

properCS :: WrappedCS cs -> Bool
properCS (JustCS _) = True
properCS _ = False
 
missingCS MissingCS = True
missingCS _ = False
 
invalidCS InvalidCS = True
invalidCS _ = False
 
remapInputs :: [Int] -> [WrappedCS a] -> [WrappedCS a]
remapInputs inputIndices inputs = map getinput inputIndices
  where getinput i = if i < length inputs
                     then inputs !! i
                     else InvalidCS
 
remapOutputs outputIndices outputCount outputs = 
  let idxmap = Map.fromList (zip outputIndices (if length outputs == length outputIndices
                                               then outputs
                                               else replicate (length outputIndices) InvalidCS))
  in map (\i ->  fromMaybe NullCS (Map.lookup i idxmap)) [0..outputCount - 1]
 
strictTransactor :: Transactor a -> WCSTransactor a
strictTransactor txop inputs = if all properCS inputs
                               then map JustCS $ txop (map (\(JustCS cs) -> cs) inputs)
                               else []
 
 
strictCoinKernel :: CoinKernel tx a -> WCSCoinKernel tx a
strictCoinKernel innerKernel tx = strictTransactor . innerKernel $ tx
 
 
strictMux :: WCSTransactor a -> MuxShape -> WCSTransactor a
strictMux txop (inputIndices, outputIndices, outputCount) inputs = 
  let rInputs = remapInputs inputIndices inputs
      outputs | any invalidCS rInputs = replicate outputCount InvalidCS
              | any missingCS rInputs = replicate outputCount MissingCS
              | otherwise             = txop rInputs
  in remapOutputs outputIndices outputCount outputs
 
parseMuxShape :: String -> Maybe (MuxShape, String)
parseMuxShape s = case reads s of
  [res] -> Just res
  _ -> Nothing
     
toyMuxCoinKernel :: WCSCoinKernel String cs -> WCSCoinKernel String cs
toyMuxCoinKernel innerKernel str = case parseMuxShape str of
  Just (muxShape, rest) -> strictMux (innerKernel rest) muxShape
  Nothing -> const []
 
 
parseId :: String -> Maybe (Int, String)
parseId s = case reads s of
  [res] -> Just res
  _     -> Nothing
  
toyDispatchCoinKernel :: Map.Map Int (WCSCoinKernel String cs) -> WCSCoinKernel String cs
toyDispatchCoinKernel table str = case parseId str of
  Just (opid, rest) -> case Map.lookup opid table of
    Just ke -> ke rest
    Nothing -> const []
  Nothing -> const []
                                       
transferCK :: String -> [Integer] -> [Integer]
transferCK op in_values =  let out_values :: [Integer]
                               out_values = read op
                           in if sum in_values == sum out_values
                                    then out_values
                                    else []                            

issueCK :: String -> [Integer] -> [Integer]
issueCK op in_values = if null in_values
                          then out_values
                          else []
                       where
                         out_values :: [Integer]
                         out_values = read op
