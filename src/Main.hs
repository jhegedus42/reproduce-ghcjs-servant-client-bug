{-# LANGUAGE ForeignFunctionInterface,JavaScriptFFI, CPP, QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, DeriveGeneric #-}
--{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main (
    main
) where

import Control.Monad.Trans.Either
import Servant.API
import Servant.Client
import Data.Proxy
import Control.Monad.Trans.Either
import Data.Aeson
import Data.List
import GHC.Generics

data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Show, Generic)

instance FromJSON Position
instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage
instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { name :: String
  , email :: String
  , age :: Int
  , interested_in :: [String]
  } deriving (Show, Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)

instance FromJSON Email
instance ToJSON Email

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

api :: Proxy API
api = Proxy

main = do
    putStrLn "hello"
    run


position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> EitherT ServantError IO Position

hello :: Maybe String -- ^ an optional value for "name"
      -> EitherT ServantError IO HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> EitherT ServantError IO Email

position :<|> hello :<|> marketing = client api baseUrl

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081

queries :: EitherT ServantError IO (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  msg <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, msg, em)

run :: IO ()
run = do
  res <- runEitherT queries
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, msg, em) -> do
      print pos
      print msg
      print em
