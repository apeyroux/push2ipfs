{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Control.Arrow ((>>>), (&&&))
import           Control.Lens ((^.), (^?), (^..), ix, (.~), (&))
import           Data.Aeson
import           Data.Aeson.Lens (key, _Array, _String)
import           Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import qualified Data.Text as T
import           GHC.Generics
import           Network.Wreq
import           Text.Printf

type ZoneID = T.Text
type DNSRecordID = T.Text
type Domain = T.Text
type AName = T.Text
type DNSLink = T.Text
    
data CloudFlare = CloudFlare {
      cfEndPoint :: String
    , cfKey :: String
    , cfEmail :: String
    } deriving (Show, Generic, FromJSON, ToJSON)

data UpdateDNSReq = UpdateDNSReq {
      upDnsType :: String
    , upDnsName :: AName
    , upDnsContent :: DNSLink
      } deriving (Show, Generic, FromJSON)

instance ToJSON UpdateDNSReq where
    toJSON (UpdateDNSReq dnsType dnsName dnsContent) = object ["name" .= dnsName
                                                              ,"type" .= dnsType
                                                              ,"content" .= dnsContent]

cfZoneId :: CloudFlare -> Domain -> IO (Maybe T.Text)
cfZoneId cf name = getWith (cfOpts cf & param "name" .~ [name]) (cfEndPoint cf ++ "zones")
                   >>= ((^.. responseBody . key "result" . _Array . traverse)
                       >>> (^? ix 0 . key "id" ._String)
                       >>> return)

cfDNSRecordId :: CloudFlare -> AName -> ZoneID -> IO (Maybe T.Text)
cfDNSRecordId cf aName zoneId = getWith (cfOpts cf & param "type" .~ ["TXT"] & param "name" .~ [aName])
                                (printf "%s/zones/%s/dns_records/" (cfEndPoint cf) (T.unpack zoneId))
                                >>=  ((^.. responseBody . key "result" . _Array . traverse)
                                     >>> (^? ix 0 . key "id" ._String)
                                     >>> return)

upDnsLink :: CloudFlare -> ZoneID -> DNSRecordID -> DNSLink -> AName -> IO Int
upDnsLink cf zoneId dnsRecordId dnsLink aName = putWith (cfOpts cf)
                                                (printf "%s/zones/%s/dns_records/%s" (cfEndPoint cf) (T.unpack zoneId) dnsRecordId)
                                                (toJSON $ UpdateDNSReq "TXT" aName dnsLink)
                                                >>= ((^. responseStatus . statusCode) >>> return)
                                                                            
cfOpts :: CloudFlare -> Options
cfOpts cf = defaults & header "Content-Type" .~ ["application/json"]
         & header "X-Auth-Email" .~ [C8.pack $ cfEmail cf]
         & header "X-Auth-Key" .~ [C8.pack $ cfKey cf]
     
main :: IO ()
main = do
  cfData <- BL.readFile "/home/alex/.cf.json" >>= return . fromJust . decode :: IO CloudFlare
  zoneID <- cfZoneId cfData "px.io"
  dnsID <- cfDNSRecordId cfData "alex.px.io" $ fromJust zoneID
  upDnsLink cfData (fromJust zoneID) (fromJust dnsID)
       "dnslink=/ipns/QmZ2DdbguyfrLXDhGd3dxAbEP7eeXcJofVpSwovVVhjYAu" "alex.px.io" >>= print
