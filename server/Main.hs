{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Main where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Base64 as BS64
import Data.List
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Common.Text
import Servant.Server
import Network.Wai (Request)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Method
import qualified Crypto.Hash.SHA256 as SHA256

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
    toJSON Unauthenticated   = object [ "success" .= False ]

-- | HTTP Headers
newtype HttpHeader = HttpHeader Text
    deriving (Eq, Show, FromText, ToText)

-- | String denoting computing resources.
type CompResource = String

-- | Results of resource lookup.
data GetResourceResult = FoundResources [CompResource] | NoResources | NoUser deriving (Show)

instance ToJSON GetResourceResult where
    toJSON (FoundResources r) = object [ "success"   .= True
                                       , "resources" .= r ]
    toJSON NoResources        = object [ "success"   .= True
                                       , "resources" .= ([] :: CompResource) ]
    toJSON NoUser             = object [ "success" .= False ]

--------------------------------------------------------------------------------
                   -- POST /auth
                   -- Authenticate a given consumer username & password
                   -- against a given resource
                   -- Returns a 201 Created
type MetaDataAPI = "auth"     :> ReqBody AuthPack
                              :> Post AuthResult
                   -- GET /resource
                   -- Get resources for a current token
              :<|> "resource" :> Header "authorization" HttpHeader
                              :> Get GetResourceResult

server :: Server MetaDataAPI
server = doAuth :<|> doGetResources

doAuth :: AuthPack -> EitherT (Int, String) IO AuthResult
doAuth ap = liftIO $ tryAuth ap

doGetResources :: Maybe HttpHeader -> EitherT (Int, String) IO GetResourceResult
doGetResources t = liftIO $ tryGetResources t

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
                    return $ Authenticated username (makeKey username p) t
                else return Unauthenticated
        _      -> return Unauthenticated

-- | Make me a key.
-- Don't do this in production.
makeKey :: String -> String -> String
makeKey u p = unpack $ BS64.encode $ SHA256.hash $ pack $ u <> p

-- | Gets a username from a key using a stupid lousy slow method.
-- Don't do this in production.
isOkKey :: String -> IO (Maybe String)
isOkKey ('O':'A':'u':'t':'h':' ':k) = return $ lookup k allKeys
  where
    allKeys = map toKey defaultUsers
    toKey (u,p) = (makeKey u p, u)

-- | Try and get resources that matches a user that owns a given token.
tryGetResources :: Maybe HttpHeader -> IO GetResourceResult
tryGetResources (Just (HttpHeader k)) = do
    maybeUsername <- isOkKey (T.unpack k)
    case maybeUsername of
        Just u -> do
            case lookup u defaultUserResources of
                Just r -> do
                    return $ FoundResources r
                _      -> return $ NoResources
        _      -> return NoUser
tryGetResources _ = return NoUser

-- | Hardcoded credentials
defaultUsers :: [(String, String)]
defaultUsers = [ ("jim@anchor.net.au", "bob")
               , ("hi@anchor.net.au", "there")
               , ("jane@anchor.net.au", "doe")
               , ("hoob@anchor.net.au", "adoob")]

-- | User resources
defaultUserResources :: [(String, [CompResource])]
defaultUserResources = [ ("jim@anchor.net.au", [ "server:banjo"
                                               , "server:timpani"])
                       , ("jane@anchor.net.au", [ "server:gamelan"
                                                , "server:theremin"
                                                , "server:lfo"]) ]

--------------------------------------------------------------------------------
-- | Run app
main :: IO ()
main = do
    putStrLn "Running on http://localhost:8080"
    Network.Wai.Handler.Warp.run 8080 $
        logStdoutDev $
        cors myCors $
        handleOptions $
        serve metaAPI server

-- | My CORS policy
myCors :: Request -> Maybe CorsResourcePolicy
myCors _ = Just $ simpleCorsResourcePolicy {
       corsOrigins = Just (["http://localhost:8000"], True)
     , corsMethods = [methodGet, methodPost, methodOptions]
     , corsRequestHeaders = ["Content-Type", "Authorization"]
     , corsVaryOrigin = True
     , corsRequireOrigin = True
     }
