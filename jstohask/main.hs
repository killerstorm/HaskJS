{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import Debug.Trace

type TxId = String
data Tx = Tx { payload       :: String, 
               inputIndices  :: [Int],
               outputIndices :: [Int], 
               outputCount   :: Int,
               txId          :: TxId,
               inputs        :: [(TxId, Int)]-- list of input transactions IDs
             } 

instance Eq Tx where
   a == b = if txId a == txId b
            then True
            else False

instance Show Tx where
  show a = show  $ txId a ++ " >>"
  

maybeIndex :: [Maybe a] -> Int -> Maybe a
maybeIndex list idx = if idx < length list
                      then list !! idx
                      else Nothing
 
reshape :: [a] -> [Int] -> Int -> [Maybe a]
reshape list newIndices length = let imap = Map.fromList (zip newIndices list)
                                 in map (`Map.lookup` imap) [0..length-1]
 
applyColorKernel :: Tx -> [Maybe Integer] -> [Maybe Integer]
applyColorKernel   tx in_values = let selectedInValues  = map (maybeIndex in_values) (inputIndices tx)
                                      inputsAreGood     = all (maybe False (\x -> True)) selectedInValues
                                      outputIndicesOK   = all ((>) $ outputCount tx) (outputIndices tx)
                                      allCorrect        = inputsAreGood && outputIndicesOK
                                      kOutValues        = trivialColorKernel (payload tx) (map (\(Just x) -> x) selectedInValues)
                                  in if allCorrect
                                     then reshape kOutValues (outputIndices tx) (outputCount tx)
                                     else replicate (outputCount tx) Nothing
                                      
 
trivialColorKernel :: String -> [Integer] -> [Integer]
trivialColorKernel op in_values = let out_values = read op :: [Integer]
                                  in if sum in_values == sum out_values 
                                     then out_values
                                     else []

topologicalSort :: [Tx] -> [Tx]-> [Tx]                          
topologicalSort g tx = tsort tx []        
                       where tsort [] r           = r
                             tsort (x:xs) visited
                               | elem x visited   = tsort xs visited   -- Tx already visited
                               | otherwise        = tsort xs (x : (tsort (filter (\t -> elem (txId t) $ map fst (inputs x)) g) visited))
                                                                                                    
tx1 = Tx "[1]" [0] [0] 2 "1" [("7", 8)]
tx2 = Tx "[3]" [0] [0] 0 "2" [("1", 8)]    
tx3 = Tx "[4]" [0] [0] 0 "3" [("1", 8)] 
tx4 = Tx "[5]" [0] [0] 0 "4" [("2", 8), ("3", 8)]
tx5 = Tx "[6]" [0] [0] 0 "5" [("8", 8)]
tx6 = Tx "[7]" [0] [0] 0 "6" [("3", 8)]

main = print $ topologicalSort [tx2, tx3, tx1, tx4, tx5, tx6] [tx4]
