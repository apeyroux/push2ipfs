{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.IPFS where

import           Control.Arrow ((>>>), (&&&))
import           Control.Lens ((^.), (^?), (^..), ix, (.~), (&))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens (key, _Array, _String)
import           Data.Map
import           Data.Monoid
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.Wreq
import           System.Directory
import           System.FilePath.Posix
import           Text.Printf

type IPFSPath = T.Text

newtype IPFS = IPFS {
  ipfsEndPoint :: String
  } deriving (Show)

data IPFSObj = IPFSObj {
  ipfsId :: IPFSPath
  , ipfsLocalPath :: FilePath
  } deriving (Show)

ipfsDefaultConfig :: IPFS
ipfsDefaultConfig = IPFS "http://127.0.0.1:5001/api/v0/"

ipfsPing :: IO Int
ipfsPing = get (printf "%s/ping" (ipfsEndPoint ipfsDefaultConfig)) >>= ((^. responseStatus . statusCode) >>> return)

ipfsAdd :: FilePath -> IO IPFSObj
ipfsAdd fp = do
  q <- postWith opts (printf "%s/add" (ipfsEndPoint ipfsDefaultConfig)) (partFile "file" fp)
  return $ IPFSObj (q ^. responseBody . key "Hash" . _String) fp
  where
    opts = defaults & param "recursive" .~ ["true"]
           & param "wrap-with-directory" .~ ["true"]
           & param "quieter" .~ ["true"]

ipfsRecAdd :: FilePath -> IO [IPFSObj]
ipfsRecAdd fp = lsR fp >>= mapM ipfsAdd

lsR :: FilePath -> IO [FilePath]
lsR fp = listDirectory fp
         >>= mapM (return . (</>) fp)
         >>= mapM (\f -> do
                      isExist <- doesDirectoryExist f
                      if isExist then
                        lsR f
                      else
                        return [f]
                  ) >>= (return . mconcat)

ipfsPublish :: IPFSPath -> IO T.Text
ipfsPublish ip = do
  q <- getWith opts (printf "%s/name/publish" (ipfsEndPoint ipfsDefaultConfig))
  return $ q ^. responseBody . key "Name" . _String
  where
    opts = defaults & param "arg" .~ [ip]
