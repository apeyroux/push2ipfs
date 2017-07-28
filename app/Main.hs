{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Network.CloudFlare
import Data.Aeson
import qualified Data.Text as T
     
main :: IO ()
main = do
  cfData <- BL.readFile "/home/alex/.cf.json" >>= return . fromJust . decode :: IO CloudFlare
  zoneID <- cfZoneId cfData "px.io"
  dnsID <- cfDNSRecordId cfData "alex.px.io" $ fromJust zoneID
  upDnsLink cfData (fromJust zoneID) (fromJust dnsID)
       "dnslink=/ipns/QmZ2DdbguyfrLXDhGd3dxAbEP7eeXcJofVpSwovVVhjYAu" "alex.px.io" >>= print
