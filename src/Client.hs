{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Client (initClient, get, Response (..), SanityConfig (..)) where

import System.Environment.MrEnv (envAsString)
import System.Exit

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe
import Data.Text                  (pack)
import GHC.Generics               hiding (Meta)
import Prelude                    hiding (filter, id)

data Meta = Meta { count :: Int
                 , total :: Int
                 , filter :: String
                 } deriving (Show, Generic, FromJSON, ToJSON)

data Data = Data { id :: Int
                 } deriving (Show, Generic, FromJSON, ToJSON)

data Response = Response { _meta :: Meta
                         , _data :: [Data]
                         } deriving (Generic, FromJSON, ToJSON)

data SanityConfig = SanityConfig { projectId :: String
                                 , dataset :: String
                                 , token :: String
                                 } deriving (Show, Generic)

-- Init func for client config state
initClient :: IO SanityConfig
initClient = do
  projectId <- envAsString "SANITY_PROJECT_ID" ""
  dataset <- envAsString "SANITY_DATASET" ""
  token <- envAsString "SANITY_TOKEN" ""
  return SanityConfig { projectId = projectId
                      , dataset = dataset
                      , token = token
                      }

get query = do
  -- Temp response lol
  let result = encode Response { _meta = Meta { count = 0
                                             , total = 0
                                             , filter = "'" <> query <> "'"
                                             }
                               , _data = [Data { id = 1 }
                                        , Data { id = 2 }
                                         ] 
                               }
  result :: ByteString

-- urlFor assetId = do
--   baseUrl <- envAsString "BASE_URL" ""
--   return $ baseUrl <> "/assets/" <> assetId :: IO ByteString
