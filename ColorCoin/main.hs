{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.Prim 
import Haste.JSON as J
--import Haste.Parsing
--import Haste.Serialize
import Data.Either
import Control.Applicative
import System.Random as SR


import qualified Data.Map as Map

import Debug.Trace

import CoinKernel
import TransactionGraph

instance Pack JSON
instance Unpack JSON
instance Pack Integer
instance Unpack Integer
instance Pack (WrappedCS Integer)
instance Unpack (WrappedCS Integer)

newtype Seed = Seed (Int, SR.StdGen)

pick :: [a] -> a
pick xs = (xs !!) $  fst $ Haste.randomR (0, length xs - 1) (mkSeed 5)


apply' = (applyTx (toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK)),
                                                 (1, (strictCoinKernel issueCK))    ]))))

kernel = toyMuxCoinKernel
           (toyDispatchCoinKernel (Map.fromList [(0, (strictCoinKernel transferCK)),
                                                 (1, (strictCoinKernel issueCK))    ]))

packToString m = Prelude.foldr f [] $ Map.toList m
  where f x acc = (: acc) $ 
                  "{" ++ "\"txID\""       ++ ":" ++ "\"" ++ a ++ "\"" ++ "," ++
                         "\"index\""      ++ ":" ++ b ++ "," ++
                         "\"coinState\""  ++ ":" ++ "\"" ++ c ++ "\"" ++ "}"              
                  where 
                        a = fst $ fst x
                        b = show . snd $ fst x
                        c = show $ snd x 

runCoinKernelOnGraph :: [(String, [CoinId], TxId, Int)] -> IO [String]
runCoinKernelOnGraph xs = return . packToString $ foldTxGraph g apply'
  where g = Prelude.foldl (\acc (a, b, c, d) -> (Tx a b c d) : acc) [] xs
                        
getMuxShape :: String -> IO String
getMuxShape payload = return $ ms
  where ms = case parseMuxShape payload of
          Just x -> show $ fst x
          _      -> "Nothing"

runCoinKernel :: (String, [(CoinId, Integer)]) -> IO [String]
runCoinKernel (x, xs) = return $ "":[] -- $ map (\x -> (show $ fst x) ++ (show $ snd x)) $  (coins 10 xs)
  where coins count unspent | count == 0  = unspent
                            | otherwise   = coins (count - 1) (Map.toList
                                                           (foldTxGraph (getGraph unspent) apply') ++ unspent)
        
--TODO: Random coins pick. Create transactions with random payload. Run coinKernel and save result to unspent.
--getGraph xs   =  (Tx (getPayload p) (inputs : []) "9" 1) : []
--  where
--                     p      = pick xs
--                     inputs = fst $ p
--                     
--getPayload xs =  "([], [0], 1) 1 " ++ (show $ (snd xs) : [])

getRandomNum :: Int -> IO [Int]
getRandomNum n = getRand n []
  where getRand counter acc
              | counter == 0     = return acc
              | otherwise        = do
                     g <- SR.newStdGen
                     let t = head $ SR.randomRs (1, n) g
                     --print t
                     getRand (counter - 1) $ t  : acc




main = do
  export (toJSStr "runCoinKernelOnGraph") runCoinKernelOnGraph
  export (toJSStr "runCoinKernel") runCoinKernel
  export (toJSStr "getMuxShape") getMuxShape
  export (toJSStr "getRandomNum") getRandomNum
