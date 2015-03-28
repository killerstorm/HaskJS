module TransactionGraph where

import CoinKernel
import qualified Data.Map as Map
  
import Debug.Trace


topologicalSort :: (Show a) => [Tx a] -> [Tx a]-> [Tx a]                        
topologicalSort g tx = tsort tx []        
  where tsort [] r           = r                  -- return sorted list
        tsort (x:xs) visited
          | elem x visited   = tsort xs visited   -- Tx already visited
          | otherwise        = tsort xs $
                               x : tsort (filter (\t -> elem (txId t) $ map fst (inputs x)) g) visited


-- ???? applyTx :: (ColorTx txPayload -> [coinState] -> [coinState]) ->
--                  Tx txPayload -> CoinStateMap coinState -> CoinStateMap coinState
applyTx  kernel tx csMap   = Map.union csMap (Map.fromList validOutputCoins)
  where ins                = map (\x -> case Map.lookup x csMap of
                                     Nothing        -> MissingCS
                                     Just x         -> x) (inputs tx)
        outputs            = kernel (payload tx) ins
        coinIds            = zip (repeat $ txId tx) [0..]
        outputCoins        = zip coinIds outputs
        validOutputCoins   = filter (notMissingCS . snd) outputCoins

notMissingCS :: WrappedCS cs -> Bool
notMissingCS MissingCS = False
notMissingCS _         = True

foldTxGraph :: [a] -> (a -> Map.Map k v -> Map.Map k v) -> Map.Map k v
foldTxGraph g apply = foldl applyTx' Map.empty g 
  where applyTx' acc tx = apply tx acc

--topologicalSort :: (Show a) => [Tx a] -> [Tx a]-> [Tx a]                        
topologicalSort' g keys = tsort keys (Map.empty, [])       
  where
    tsort [] (visited, acc)        = (visited, acc)
    tsort (x:xs) (visited, acc)
          | Map.member x visited   = tsort xs (visited, acc)   -- Tx already visited
          | otherwise          = tsort xs $ (Map.insert x' b visited', x'' ++ acc')
                                   where
                                     b@(ins, p, _) = lookup' x g
                                     x' = if p == "" then "" else x
                                     x'' = if p == "" then [] else [(x, b)]
                                     (visited', acc') =  tsort k (visited, acc)
                                     k = Prelude.map (\x -> fst x) ins

                                            
lookup' k m = case Map.lookup k m of
  Just x      -> x
  Nothing     -> ([],"", 0)
