{-# LANGUAGE OverloadedStrings #-}

module OptionsApp (
    handleOptions
) where

import Prelude hiding (lookup)
import Control.Monad
import Data.ByteString
import Data.List
import Data.Maybe
import Network.Wai
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Method

-- | All OPTIONS queries get a 200 OK with a blank body. YEAH!
handleOptions :: Middleware
handleOptions app req respond =
    case requestMethod req of
        "OPTIONS" -> respond $
            responseLBS H.status200 rh ""
        _         -> app req respond
  where
    rh = [("Access-Control-Allow-Origin", getReferer)]
    getReferer = fromMaybe "http://localhost:8000" $ lookup "referer" $ requestHeaders req
