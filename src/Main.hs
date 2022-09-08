{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import qualified Client as Sanity
import Client (Response (..), SanityConfig (..))

import Control.Monad              (when)
import Control.Monad.Trans
import Data.Aeson                 hiding (json)
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.IORef
import Data.List                  (genericLength)
import Data.Monoid                ((<>))
import Data.Maybe                 (fromMaybe, isNothing)
import Data.String                (fromString, IsString)
-- import Data.Text                  (Text)
import GHC.Generics

import System.Environment.MrEnv (envAsString)
import System.Exit

import Web.Spock
import Web.Spock.Action
import Web.Spock.Config

-- types
type Api = SpockM () () ApiState

-- newtype ApiState = ApiState (IORef [String])
data ApiState = ApiState { sanityConfig :: IORef (IO Sanity.SanityConfig)
                         , accessTokens :: IORef [String]
                         } deriving (Generic)

data JsonRes = JsonRes
  { code :: Int
  , message :: String
  , content :: String
  } deriving (Generic)

data SanityRes = SanityRes String deriving (Generic)

instance ToJSON JsonRes
instance FromJSON JsonRes

instance ToJSON SanityRes
instance FromJSON SanityRes

app :: Api ()
main :: IO ()

-- misc
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
            "" -> []
            s' -> w : wordsWhen p s''
                  where (w, s'') = break p s'

-- main
main = do
  putStrLn "Starting..."
  -- get token(s), store in List
  str <- envAsString "ACCESS_TOKENS" ""

  let tokens = wordsWhen (==',') str
  let config = Sanity.initClient

  -- check token(s) exist
  when (genericLength tokens < 1) $ do
      putStrLn "Error: No tokens found."
      -- die "Exiting..."

  -- check config
  SanityConfig { projectId, dataset, token } <- config
  when (projectId == "" || dataset == "" || token == "") $ do
      putStrLn "Error: Sanity config not set."
      -- die "Exiting..."

  putStrLn $ "Using token(s): " <> show tokens
  tokensRef <- newIORef tokens
  configRef <- newIORef config
  let apiState = ApiState { sanityConfig = configRef
                          , accessTokens = tokensRef
                          }
  -- Spock instance using routes / state
  spockCfg <- defaultSpockCfg () PCNoDatabase apiState
  runSpock 8888 (spock spockCfg app)

-- routes
app = do
      get baseRoute baseAction
      get queryRoute $ \path -> queryAction path
      get cdnRoute $ \path -> cdnAction path

      hookAny GET $ const notFoundAction
      hookAny POST $ const notFoundAction

-- routes
baseRoute = "/"
apiRoute = baseRoute <//> "api/v1"

queryRoute = apiRoute <//> "query" <//> (var :: Var String)
cdnRoute = "cdn" <//> (var :: Var String)

-- actions

-- TODO: error handling actions, check bearer token, etc.

baseAction =
  json $ JsonRes { code = 418, message = "I'm a teapot", content = "" }

notFoundAction =
  json $ JsonRes { code = 404, message = "Requested resource not found or inaccessible", content = "" }

queryAction path =
  -- json $ decode (Sanity.get "test")
  -- json $ JsonRes { code = 200, message = "OK", content = (unpack (Sanity.get "test")) }
  -- This is dumb, need to figure out how to properly decode JSON ByteString res to data struct
  json $ SanityRes (unpack (Sanity.get path))
cdnAction path =
  json $ JsonRes { code = 200, message = "Placeholder res for now, path: " <> path <> "...", content = "" }
