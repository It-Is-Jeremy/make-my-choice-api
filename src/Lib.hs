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
import Database.PostgreSQL.Simple.Types
import qualified Data.Text as Text
import Control.Monad

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

getElements :: Connection -> UUID -> IO [Only (PGArray Text.Text)]
getElements conn id = query conn "Select elements from set_table where id=?" (Only ((toString id) :: String))


setServer :: Server SetApi
setServer = getSet :<|> createSet
  where getSet :: UUID -> Handler Set
        getSet _setId = do
          connection <- liftIO getConnection
          fromRows <- liftIO (getElements connection _setId)
          let mappedRows = map fromOnly (fromRows :: [Only (PGArray Text.Text)])
          let mappedLists = fromPGArray (head mappedRows)
          let resultElements = map Text.unpack (mappedLists)
          return (Set _setId (resultElements))

        createSet :: [String] -> Handler Set
        createSet setElements = do
          connection <- liftIO getConnection
          uuid <- liftIO (nextRandom)
          res <- liftIO (execute connection "insert into set_table (id, elements) values (?, ?)" ((toString uuid) :: String, PGArray (setElements)))
          let newSet = Set uuid setElements
          return newSet

startApp :: IO ()
startApp = run 8080 app

api :: Proxy SetApi
api = Proxy

app :: Application
app = serve api setServer
