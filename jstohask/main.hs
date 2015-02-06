{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map

 
--type ColorKernel = String -> [coinState] -> [coinState]
data Tx = Tx { payload       :: String, 
               inputIndices  :: [Int],
               outputIndices :: [Int], 
               outputCount   :: Int}

 
maybeIndex :: [Maybe a] -> Int -> Maybe a
maybeIndex list idx = if idx < length list
                      then list !! idx
                      else Nothing
 
reshape :: [a] -> [Int] -> Int -> [Maybe a]
reshape list newIndices length = let imap = Map.fromList (zip newIndices list)
                                 in map (\i -> Map.lookup i imap) [0..length-1]
 
applyColorKernel :: (String -> [Integer] -> [Integer]) -> Tx -> [Maybe Integer] -> [Maybe Integer]
applyColorKernel k tx in_values = let selectedInValues  = map (maybeIndex in_values) (inputIndices tx)
                                      inputsAreGood     = and (map (maybe False (\x -> True)) selectedInValues)
                                      outputIndicesOK   = and (map ((>) (outputCount tx)) (outputIndices tx))
                                      allCorrect        = inputsAreGood && outputIndicesOK
                                      kOutValues        = k (payload tx) (map (\(Just x) -> x) selectedInValues)
                                  in if allCorrect
                                     then reshape kOutValues (outputIndices tx) (outputCount tx)
                                     else replicate (outputCount tx) Nothing
                                      
 
trivialColorKernel :: String -> [Integer] -> [Integer]
trivialColorKernel op in_values = let out_values :: [Integer]
                                      out_values = read op
                                  in if (sum in_values) == (sum out_values) 
                                     then out_values
                                     else []                            

main = print (applyColorKernel trivialColorKernel tx inputs)
       where tx = Tx { payload = "[1]", inputIndices = [0], outputIndices = [0], outputCount = 1}
             inputs = [Just 1]
