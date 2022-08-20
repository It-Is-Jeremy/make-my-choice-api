{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import GHC.TypeLits
import Control.Monad.IO.Class (liftIO)
import Data.UUID
import Data.UUID.V4
import Database.PostgreSQL.Simple

data Set = Set
  {
    setId    :: UUID
  , elements :: [String]
  } deriving Generic

instance ToJSON Set

type SetApi = "set" :>
                    Capture "id" UUID :> Get '[JSON] Set
                 :<|> "set" :>
                    ReqBody '[JSON] [String] :> Post '[JSON] Set

setServer :: Server SetApi
setServer = getSet :<|> createSet
  where getSet :: UUID -> Handler Set
        getSet _setId = return (Set _setId ["Jeremy", "Ash"])

        createSet :: [String] -> Handler Set
        createSet setElements = do
          connection <- liftIO (connectPostgreSQL "postgresql://postgres:postgres@db")
          uuid <- liftIO (nextRandom)
          let newSet = Set uuid setElements
          return newSet

startApp :: IO ()
startApp = run 8080 app

api :: Proxy SetApi
api = Proxy

app :: Application
app = serve api setServer
