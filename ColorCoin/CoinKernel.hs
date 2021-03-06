module CoinKernel where


import Debug.Trace

import qualified Data.Map as Map

type TxId = String
type CoinId = (TxId, Int)
data Tx a = Tx { payload :: a, inputs :: [CoinId], txId :: TxId, outputCount :: Int} deriving Show
  
instance Eq (Tx a) where
  a == b = txId a == txId b
  
data WrappedCS cs = JustCS cs | MissingCS | InvalidCS | NullCS
 
instance Show cs => Show (WrappedCS cs) where
  show wcs = case wcs of 
    JustCS cs -> show cs
    MissingCS -> "missing"
    InvalidCS -> "invalid"
    NullCS -> "null"

type MuxShape = ([Int], [Int], Int)
type Transactor cs = [cs] -> [cs]
type WCSTransactor cs = Transactor (WrappedCS cs)
 
type CoinKernel tx cs = tx -> Transactor cs
type WCSCoinKernel tx cs = tx -> WCSTransactor cs
 
properCS :: WrappedCS cs -> Bool
properCS (JustCS _) = True
properCS _ = False
 
missingCS MissingCS = True
missingCS _ = False
 
invalidCS InvalidCS = True
invalidCS _ = False
 
remapInputs :: [Int] -> [WrappedCS a] -> [WrappedCS a]
remapInputs inputIndices inputs = map getinput inputIndices
  where getinput i = if (i < length inputs)
                     then inputs !! i
                     else InvalidCS
 
remapOutputs outputIndices outputCount outputs = 
  let idxmap = Map.fromList (zip outputIndices (if (length outputs) == (length outputIndices)
                                               then outputs
                                               else (replicate (length outputIndices) InvalidCS)))
  in map (\i -> case Map.lookup i idxmap of
             Just output -> output
             Nothing -> NullCS) [0..outputCount - 1]
 
strictTransactor :: Transactor a -> WCSTransactor a
strictTransactor txop inputs = if (all properCS inputs)
                               then map JustCS $ txop (map (\(JustCS cs) -> cs) inputs)
                               else []
 
 
strictCoinKernel :: CoinKernel tx a -> WCSCoinKernel tx a
strictCoinKernel innerKernel = \tx -> strictTransactor (innerKernel tx)
 
 
strictMux :: WCSTransactor a -> MuxShape -> WCSTransactor a
strictMux txop (inputIndices, outputIndices, outputCount) inputs = 
  let rInputs = remapInputs inputIndices inputs
      outputs | any invalidCS rInputs = replicate outputCount InvalidCS
              | any missingCS rInputs = replicate outputCount MissingCS
              | otherwise             = txop rInputs
  in remapOutputs outputIndices outputCount outputs
 
parseMuxShape :: String -> Maybe (MuxShape, String)
parseMuxShape s = case (reads s) of
  [res] -> Just res
  _ -> Nothing
     
toyMuxCoinKernel :: WCSCoinKernel String cs -> WCSCoinKernel String cs
toyMuxCoinKernel innerKernel = \str-> case parseMuxShape str of
  Just (muxShape, rest) -> strictMux (innerKernel rest) muxShape
  Nothing -> const []
 
 
parseId :: String -> Maybe (Int, String)
parseId s = case (reads s) of
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
                           in if (sum in_values) == (sum out_values) 
                                    then out_values
                                    else []                            

issueCK :: String -> [Integer] -> [Integer]
issueCK op in_values = if null in_values
                          then out_values
                          else []
                       where
                         out_values :: [Integer]
                         out_values = read op
