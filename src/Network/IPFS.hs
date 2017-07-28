{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.IPFS where

import Network.Wreq
import Text.Printf
import           Control.Arrow ((>>>), (&&&))
import           Control.Lens ((^.), (^?), (^..), ix, (.~), (&))

newtype IPFS = IPFS {
  ipfsEndPoint :: String
  } deriving (Show)

ipfsDefaultConfig :: IPFS
ipfsDefaultConfig = IPFS "http://127.0.0.1:5001/api/v0/"

ipfsPing :: IO Int
ipfsPing = get (printf "%s/ping" (ipfsEndPoint ipfsDefaultConfig)) >>= ((^. responseStatus . statusCode) >>> return)
