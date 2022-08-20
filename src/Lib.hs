{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
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
          uuid <- liftIO (nextRandom)
          return (Set uuid setElements)

startApp :: IO ()
startApp = run 8080 app

api :: Proxy SetApi
api = Proxy

app :: Application
app = serve api setServer
