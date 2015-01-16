{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Network.Wai (Request)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Method

import OptionsApp

--------------------------------------------------------------------------------
-- | Packs username and password login requests into a consistent structure
data AuthPack = AuthPack {
    username :: String,
    password :: String
}

instance FromJSON AuthPack where
    parseJSON (Object o) = AuthPack <$> o .: "username" <*> o .: "password"

-- | Results of login attempts: either you're authenticated, or you're not.
data AuthResult = Authenticated {
    identity :: String,
    token    :: String,
    expiry   :: UTCTime
} | Unauthenticated

instance ToJSON AuthResult where
    toJSON Authenticated{..} = object [ "success" .= True
                                      , "identity" .= identity
                                      , "token"    .= token
                                      , "expiry"   .= expiry ]
    toJSON Unauthenticated = object [ "success" .= False ]

--------------------------------------------------------------------------------
                   -- POST /auth
                   -- Authenticate a given consumer username & password
                   -- against a given resource
                   -- Returns a 201 Created
type MetaDataAPI = "auth"     :> ReqBody AuthPack
                              :> Post AuthResult

server :: Server MetaDataAPI
server = doAuth
  where
    doAuth = liftIO . tryAuth

metaAPI :: Proxy MetaDataAPI
metaAPI = Proxy

--------------------------------------------------------------------------------
-- | Try and authenticate user against a hardcoded list of credentials
tryAuth :: AuthPack -> IO AuthResult
tryAuth AuthPack{..} =
    case lookup username defaultUsers of
        Just p ->
            if p == password
                then do
                    t <- getCurrentTime
                    return $ Authenticated username "borp" t
                else return Unauthenticated
        _      -> return Unauthenticated

-- | Hardcoded credentials
defaultUsers :: [(String, String)]
defaultUsers = [ ("jim@anchor.net.au", "bob")
               , ("hi@anchor.net.au", "there")
               , ("jane@anchor.net.au", "doe")
               , ("hoob@anchor.net.au", "adoob")]

--------------------------------------------------------------------------------
-- | Run app
main :: IO ()
main = do
    putStrLn "Running on http://localhost:8080"
    Network.Wai.Handler.Warp.run 8080 $
        logStdoutDev $
        cors myCors $
        handleOptions $
        serve fleetMetaAPI server

-- | My CORS policy
myCors :: Request -> Maybe CorsResourcePolicy
myCors _ = Just $ simpleCorsResourcePolicy {
       corsOrigins = Just (["http://localhost:8000"], True)
     , corsMethods = [methodGet, methodPost, methodOptions]
     , corsRequestHeaders = ["Content-Type"]
     , corsVaryOrigin = True
     , corsRequireOrigin = True
     }
