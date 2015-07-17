{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Src.Types where

import Haste.Serialize
import Haste.JSON

type TxId = String
type CoinId = (TxId, Int)

data Tx a =  Tx { payload     :: a,
                  inputs      :: [CoinId],
                  txId        :: TxId,
                  outputCount :: Int} deriving Show

instance (Serialize a) => Serialize (Tx a) where
  toJSON (Tx payload inputs txid outputCount) = Dict [("payload", toJSON payload),
                                                      ("inputs", Arr (map toJSON inputs)),
                                                      ("txid", toJSON txid),
                                                      ("outputCount", toJSON outputCount)]
  parseJSON j = do
    payload     <- j .: "payload"
    inputs      <- j .: "inputs"
    txid        <- j .: "txid"
    outputCount <- j .: "outputCount"
    return $ Tx payload inputs txid outputCount
  
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
 
data Coin = Coin {
  txid      :: String,
  index     :: Int,
  value     :: Integer,
  coinstate :: String
               } deriving Show

instance Serialize Coin where
  toJSON (Coin txid index value coinstate) = Dict [("txid", toJSON txid),
                                                   ("index", toJSON index),
                                                   ("value", toJSON value),
                                                   ("coinstate", toJSON coinstate)]
  parseJSON j = do
    txid      <- j .: "txid"
    index     <- j .: "index"
    value     <- j .: "value"
    coinstate <- j .: "coinstate"
    return $ Coin txid index value coinstate

instance Serialize Integer where
  toJSON = Num . fromIntegral
  parseJSON (Num x) =
    case truncate x of
      x' | fromIntegral x' == x ->
        return x'
      _ ->
        fail "The given Number can't be represented as an Integer"
  parseJSON _ =
    fail "Tried to deserialize a non-Number to an Integer"
