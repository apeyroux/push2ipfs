{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.IPFS where

import           Control.Arrow ((>>>), (&&&))
import           Control.Lens ((^.), (^?), (^..), ix, (.~), (&))
import           Data.Aeson
import           Data.Aeson.Lens (key, _Array, _String)
import           Data.Map
import Data.Monoid
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.Wreq
import           Text.Printf

import Control.Monad

import System.Directory
import System.FilePath.Posix

newtype IPFS = IPFS {
  ipfsEndPoint :: String
  } deriving (Show)

ipfsDefaultConfig :: IPFS
ipfsDefaultConfig = IPFS "http://127.0.0.1:5001/api/v0/"

ipfsPing :: IO Int
ipfsPing = get (printf "%s/ping" (ipfsEndPoint ipfsDefaultConfig)) >>= ((^. responseStatus . statusCode) >>> return)

ipfsAdd :: FilePath -> IO T.Text
ipfsAdd fp = do
  q <- postWith opts (printf "%s/add" (ipfsEndPoint ipfsDefaultConfig)) (partFile "file" fp)
  return $ q ^. responseBody . key "Hash" . _String
  where
    opts = defaults & param "recursive" .~ ["true"]
           & param "wrap-with-directory" .~ ["true"]
           & param "quieter" .~ ["true"]

ipfsRecAdd :: FilePath -> IO [T.Text]
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
