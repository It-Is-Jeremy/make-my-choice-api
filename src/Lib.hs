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
import qualified Data.Text as Text

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

getConnection :: IO Connection
getConnection = connectPostgreSQL "postgresql://postgres:postgres@db"

setServer :: Server SetApi
setServer = getSet :<|> createSet
  where getSet :: UUID -> Handler Set
        getSet _setId = do
          connection <- liftIO getConnection
          xs <- query_ connection "Select id, elements from set_table where id=?" (toString <$> _setId)
          forM_ xs $ \(id, elements) ->
            return (Set _setId elements)

        createSet :: [String] -> Handler Set
        createSet setElements = do
          connection <- liftIO getConnection
          uuid <- liftIO (nextRandom)
          execute connection "insert into set_table (id, elements) values (?, ?)" (uuid, setElements)
          let newSet = Set uuid setElements
          return newSet

startApp :: IO ()
startApp = run 8080 app

api :: Proxy SetApi
api = Proxy

app :: Application
app = serve api setServer
