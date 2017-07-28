{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.IPFS where

import           Control.Arrow ((>>>), (&&&))
import           Control.Lens ((^.), (^?), (^..), ix, (.~), (&))
import           Data.Aeson
import           Data.Aeson.Lens (key, _Array, _String)
import           Data.Map
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.Wreq
import           Text.Printf

newtype IPFS = IPFS {
  ipfsEndPoint :: String
  } deriving (Show)

ipfsDefaultConfig :: IPFS
ipfsDefaultConfig = IPFS "http://127.0.0.1:5001/api/v0/"

ipfsPing :: IO Int
ipfsPing = get (printf "%s/ping" (ipfsEndPoint ipfsDefaultConfig)) >>= ((^. responseStatus . statusCode) >>> return)

ipfsAdd :: IO T.Text
ipfsAdd = do
  q <- postWith opts (printf "%s/add" (ipfsEndPoint ipfsDefaultConfig)) (partFile "file" "/var/www/alex/index.html")
  return $ q ^. responseBody . key "Hash" . _String
  where
    opts = defaults & param "recursive" .~ ["True"]
