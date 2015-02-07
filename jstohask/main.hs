{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map

type TxId = String
data Tx = Tx { payload       :: String, 
               inputIndices  :: [Int],
               outputIndices :: [Int], 
               outputCount   :: Int,
               txId          :: TxId,
               inputs        :: [TxId],
               outputs       :: [TxId]
             } deriving Show

instance Eq Tx where
  Tx _ _ _ _ a _ _ == Tx _ _ _ _ b _ _ = True
  

maybeIndex :: [Maybe a] -> Int -> Maybe a
maybeIndex list idx = if idx < length list
                      then list !! idx
                      else Nothing
 
reshape :: [a] -> [Int] -> Int -> [Maybe a]
reshape list newIndices length = let imap = Map.fromList (zip newIndices list)
                                 in map (\i -> Map.lookup i imap) [0..length-1]
 
applyColorKernel :: Tx -> [Maybe Integer] -> [Maybe Integer]
applyColorKernel   tx in_values = let selectedInValues  = map (maybeIndex in_values) (inputIndices tx)
                                      inputsAreGood     = and (map (maybe False (\x -> True)) selectedInValues)
                                      outputIndicesOK   = and $ map ((>) $ outputCount tx) (outputIndices tx)
                                      allCorrect        = inputsAreGood && outputIndicesOK
                                      kOutValues        = trivialColorKernel (payload tx) (map (\(Just x) -> x) selectedInValues)
                                  in if allCorrect
                                     then reshape kOutValues (outputIndices tx) (outputCount tx)
                                     else replicate (outputCount tx) Nothing
                                      
 
trivialColorKernel :: String -> [Integer] -> [Integer]
trivialColorKernel op in_values = let out_values = read op :: [Integer]
                                  in if (sum in_values) == (sum out_values) 
                                     then out_values
                                     else []

topologicalSort :: [Tx] -> [Tx]
topologicalSort g = tsort [n | n <- g, inputs n == []] []
                    where tsort [] r         = r
                          tsort (x:xs) visited
                            | elem x visited   = tsort xs visited
                            | otherwise        = tsort xs (x : (tsort (filter (\t -> elem (txId t) (outputs x)) g) visited))


tx1 = Tx "[1]" [0] [0] 2 "1" []    ["2", "3"]
tx2 = Tx "[3]" [0] [0] 0 "2" ["2"] []   
tx3 = Tx "[4]" [0] [0] 0 "3" ["3"] []


main = print $ topologicalSort [tx2, tx3, tx1]
