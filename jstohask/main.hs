import qualified Data.Map as Map
 
type ColorKernel txPayload coinState = txPayload -> [coinState] -> [coinState]
data ColorTx txPayload = ColorTx { payload :: txPayload, 
                                   inputIndices :: [Int],
                                   outputIndices :: [Int], 
                                   outputCount :: Int}
 
 
type TxId = String
type CoinId = (TxId, Int)
type CoinStateMap coinState = Map.Map CoinId coinState
data Tx txPayload = Tx { colorTx :: ColorTx txPayload, inputs :: [CoinId], txId :: TxId }
 
instance Eq (Tx txPayload) where
  a == b = txId a == txId b

instance Show (Tx txPayload) where
  show tx = ">> " ++ txId tx
 
maybeIndex :: [Maybe a] -> Int -> Maybe a
maybeIndex list idx = if idx < length list
                      then list !! idx
                      else Nothing
 
reshape :: [a] -> [Int] -> Int -> [Maybe a]
reshape list newIndices length = let imap = Map.fromList (zip newIndices list)
                                 in map (\i -> Map.lookup i imap) [0..length-1]
 
applyColorKernel :: ColorKernel txPayload coinState -> ColorTx txPayload -> [Maybe coinState] -> [Maybe coinState]
applyColorKernel k tx in_values = let selectedInValues    = map (maybeIndex in_values) (inputIndices tx)
                                      inputsAreGood       = all (maybe False (\x -> True)) selectedInValues
                                      outputIndicesOK     = all ((>) (outputCount tx)) (outputIndices tx)
                                      allCorrect          = inputsAreGood && outputIndicesOK
                                      kOutValues          = k (payload tx) (map (\(Just x) -> x) selectedInValues)
                                  in if allCorrect
                                     then reshape kOutValues (outputIndices tx) (outputCount tx)
                                     else replicate (outputCount tx) Nothing
                                      
 
trivialColorKernel :: String -> [Integer] -> [Integer]
trivialColorKernel op in_values = let out_values :: [Integer]
                                      out_values = read op
                                  in if (sum in_values) == (sum out_values) 
                                     then out_values
                                     else []
                                          
                                          
topologicalSort :: [Tx txPayload] -> [Tx txPayload]-> [Tx txPayload]                        
topologicalSort g tx = tsort tx []        
                       where tsort [] r           = r                  -- return sorted list
                             tsort (x:xs) visited
                               | elem x visited   = tsort xs visited   -- Tx already visited
                               | otherwise        = tsort xs (x : (tsort (filter (\t -> elem (txId t) $ map fst (inputs x)) g) visited))


colortx = ColorTx "[7]" [0] [0] 0
                                                    
tx1 = Tx colortx  [("7", 8)]           "1"
tx2 = Tx colortx  [("1", 8)]           "2"
tx3 = Tx colortx  [("1", 8)]           "3"
tx4 = Tx colortx  [("2", 8), ("3", 8)] "4"
tx5 = Tx colortx  [("8", 8)]           "5"
tx6 = Tx colortx  [("3", 8)]           "6"
                                          
 
                                          
main = print $ topologicalSort [tx2, tx3, tx1, tx4, tx5, tx6] [tx4, tx2, tx3, tx6, tx1, tx5]
