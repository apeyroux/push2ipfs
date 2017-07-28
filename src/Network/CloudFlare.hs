{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.CloudFlare (CloudFlare
                          , cfZoneId
                          , cfDNSRecordId
                          , cfPutDnsLink
                          , cfOpts) where

import           Control.Arrow ((>>>), (&&&))
import           Control.Lens ((^.), (^?), (^..), ix, (.~), (&))
import           Data.Aeson
import           Data.Aeson.Lens (key, _Array, _String)
import           Data.ByteString.Char8 as C8 (pack)
-- import qualified Data.ByteString.Lazy as BL
-- import           Data.Maybe
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

data CloudFlarePutDNSReq = CloudFlarePutDNSReq {
      cfPutDnsType :: String
    , cfPutDnsName :: AName
    , cfPutDnsContent :: DNSLink
    } deriving (Show, Generic, FromJSON)

instance ToJSON CloudFlarePutDNSReq where
    toJSON (CloudFlarePutDNSReq dnsType dnsName dnsContent) = object ["name" .= dnsName
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

cfPutDnsLink :: CloudFlare -> ZoneID -> DNSRecordID -> DNSLink -> AName -> IO Int
cfPutDnsLink cf zoneId dnsRecordId dnsLink aName = putWith (cfOpts cf)
                                                (printf "%s/zones/%s/dns_records/%s" (cfEndPoint cf) (T.unpack zoneId) dnsRecordId)
                                                (toJSON $ CloudFlarePutDNSReq "TXT" aName dnsLink)
                                                >>= ((^. responseStatus . statusCode) >>> return)
                                                                            
cfOpts :: CloudFlare -> Options
cfOpts cf = defaults & header "Content-Type" .~ ["application/json"]
         & header "X-Auth-Email" .~ [C8.pack $ cfEmail cf]
         & header "X-Auth-Key" .~ [C8.pack $ cfKey cf]
            
