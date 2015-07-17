module Src.TransactionGraph where

import Src.CoinKernel
import Src.Types
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
applyTx  kernel tx csMap   = csmap
  where ins                = map (\x -> case Map.lookup x csMap of
                                     Nothing        -> MissingCS
                                     Just x         -> x) (inputs tx)
        outputs            = kernel (payload tx) ins
        coinIds            = zip (repeat $ txId tx) [0..]
        outputCoins        = zip coinIds outputs
        validOutputCoins   = filter (notMissingCS . snd) outputCoins
        csmap              = Map.union csMap (Map.fromList validOutputCoins)

notMissingCS :: WrappedCS cs -> Bool
notMissingCS MissingCS = False
notMissingCS _         = True

--foldTxGraph :: [a] -> (a -> Map.Map k v -> Map.Map k v) -> Map.Map k v
foldTxGraph g apply =
  foldl applyTx' Map.empty g 
  where applyTx' acc tx = apply tx acc

--topologicalSort' :: Map.Map k v -> [k] -> (Map.Map k v, [(k, v)])                        
topologicalSort' g k = tsort k (Map.empty, [])               -- g - transactions map, k - list of keys (txId list)  
  where
    tsort [] (visited, sorted)        = (visited, sorted)    -- visited - visited transactions map, sorted - sorted transactions list
    tsort (x:xs) (visited, sorted)
          | Map.member x visited || Map.notMember x g        -- if x is visited or x not in graph
                                    = tsort xs (visited, sorted) 
          | otherwise               = tsort xs (Map.insert x b visited', (x, b) : sorted') -- tsort rest of keylist, insert current tx in visited map and in sorted list
                                   where
                                     b@(ins, _, _)       = g Map.! x
                                     k                   = map fst ins                -- inputs (txids) of current transaction                  
                                     (visited', sorted') =  tsort k (visited, sorted) -- sort from current transaction


