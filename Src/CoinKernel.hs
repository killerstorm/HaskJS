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
import Data.List (nub)

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
     

data CKContext = CKContext { tx :: Tx, payload_fragment :: String }
data ToyCoinState = ToyCoinState { colorID :: String, value :: Integer }

toyMuxCoinKernel :: WCSCoinKernel CKContext cs -> WCSCoinKernel CKContext cs
toyMuxCoinKernel innerKernel ctx = case parseMuxShape (payload_fragment ctx) of
  Just (muxShape, rest) -> strictMux (innerKernel (CKContext (tx ctx) rest)) muxShape
  Nothing -> const []
 
 
parseId :: String -> Maybe (Int, String)
parseId s = case reads s of
  [res] -> Just res
  _     -> Nothing
  

toyDispatchCoinKernel :: Map.Map Int (WCSCoinKernel CKContext cs) -> WCSCoinKernel CKContext cs
toyDispatchCoinKernel table ctx = case parseId (payload_fragment ctx) of
  Just (opid, rest) -> case Map.lookup opid table of
    Just ke -> ke (CKContext (tx ctx) rest)
    Nothing -> const []
  Nothing -> const []
                                       
transferCK :: CKContext -> [ToyCoinState] -> [ToyCoinState]
transferCK ctx in_coinstates =  let out_values :: [Integer]
                                    out_values = read (payload_fragment ctx)
                                    colorIDs = nub (map colorID in_coinstates)
                                    in_values = map value in_coinstates
                                in if (length colorIDs == 1) 
                                      && (sum in_values) == (sum out_values))
                                   then map (ToyCoinState (colorIDs !! 0)) out_values
                                   else []                            

issueCK :: CKContext -> [ToyCoinState] -> [ToyCoinState]
issueCK ctx in_coinstates = if null in_coinstates
                            then map (ToyCoinState myColorID) out_values
                            else []
                        where
                          out_values :: [Integer]
                          out_values = read (payload_fragment ctx)
                          myColorID = txid (tx ctx)
