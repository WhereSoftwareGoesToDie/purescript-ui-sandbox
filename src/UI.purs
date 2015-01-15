module UI where

import Control.Monad.Eff
import Data.Either

import Debug.Trace
import Data.Foreign
import Data.Foreign.Class
import Data.Function
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Network.Ajax

import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.Cont.Trans
import Control.Monad.Trans

import Data.JSON (ToJSON, object, (.=), encode)
import Data.Options (Options(), (:=))

import qualified Network.Oboe as O

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import Debug.Trace

-- | Depicts all the actions taking place in our app.
data Action = LogIn AuthResult | LogOut

-- | Stores app state.
type State = {
    authToken :: Maybe String
}

--------------------------------------------------------------------------------
-- | Package to send to login URL.
data AuthPack = AuthPack {
    username :: String,
    password :: String
}

-- | Determines whether login attempt was successful or not.
data AuthResult = Authenticated {
                    identity :: String,
                    token    :: String,
                    expiry   :: String }
                | Unauthenticated

instance authPackToJson :: ToJSON AuthPack where
    toJSON p = object [ "username" .= p.username
                      , "password" .= p.password ]

instance foreignAuthResult :: IsForeign AuthResult where
  read value = do
    success <- readProp "success" value
    case success of
        "true" -> do
            identity <- readProp "identity" value
            token    <- readProp "token" value
            expiry   <- readProp "expiry" value
            return $ Authenticated {identity: identity, token: token, expiry: expiry}
        _ -> return Unauthenticated

--------------------------------------------------------------------------------
-- | Initial app state: logged out with no data.
initialState :: State
initialState = { authToken: Nothing }

--------------------------------------------------------------------------------
-- | Get value by selector
foreign import getSelectorValue
  "function getSelectorValue(s) {\
  \  var q = document.querySelector(s);\
  \  if (!q) return Data_Maybe.Nothing.value;\
  \  return Data_Maybe.Just.create(q.value.toString());\
  \}" :: String -> Maybe String

-- | Gets the value of the current event target.
foreign import getEventTargetValue
  "function getEventTargetValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String

--------------------------------------------------------------------------------
-- | Renders a page
render :: T.Render State _ Action
render ctx s _ = T.div [ A.className "app-container" ] [ current ]
  where
    current = if isJust s.authToken
                then loggedIn
                else loggedOut
    loggedOut = T.div' [ loginForm ]
    loggedIn = T.p' [ T.text "You're logged in!"
                    , T.button [ T.onClick ctx (\_ -> LogOut) ]
                               [ T.text "Log Out" ] ]
    loginForm = T.div'
                  [ fieldGroup [
                        T.label [] [ T.text "Email Address" ]
                      , T.input [ A._type "text"
                                , A.name "email_address"
                                , A._id "email-address"
                                , A.required "true" ]
                                []
                    ]
                  , fieldGroup [
                        T.label [] [ T.text "Password" ]
                      , T.input [ A._type "password"
                                , A.name "password"
                                , A._id "password"
                                , A.required "true" ]
                                []
                    ]
                  , fieldGroup [
                        T.button [ A._type "submit"
                                 , T.onSubmit ctx handleSubmit ]
                                 [ T.text "Log In" ]
                  ] ]
    fieldGroup children = T.div [ A.className "form-group" ] children

handleSubmit :: T.MouseEvent -> Action
handleSubmit _ =
    case Tuple eav pwv of
        (Tuple (Just u) (Just p)) ->
            lift $ tryLoginAjax u p >>=
            handleSubmitResult
        _ -> LogOut
  where
    eav = getSelectorValue "#email-address"
    pwv = getSelectorValue "#password"
    tryLoginAjax u p = ajax "http://localhost:8080/auth" $ HttpRequest {
        accepts: Json,
        contentType: JsonContent,
        method: POST,
        contents: (Just $ encode $ AuthPack {username: u, password: p})
    }

handleSubmitResult :: Either _ String -> Action
handleSubmitResult (Right text) =
    case (readJSON text :: F AuthResult) of
        Right a -> LogIn a
        Left e  -> LogOut
handleSubmitResult (Left e) = LogOut

--------------------------------------------------------------------------------
-- | Modifies the global app state based on the action taking place
performAction :: T.PerformAction _ Action (T.Action _ State) 
performAction _ (LogIn (Authenticated a)) = T.modifyState (\o ->
    { authToken: Just a.token :: Maybe String })
performAction _ (LogIn Unauthenticated) = T.modifyState (\o ->
    { authToken: Nothing :: Maybe String })
performAction _ LogOut      = T.modifyState (\o ->
    { authToken: Nothing :: Maybe String })

--------------------------------------------------------------------------------
spec :: T.Spec _ State _ Action
spec = T.Spec { initialState: initialState
              , performAction: performAction
              , render: render
              }

main = do
    let component = T.createClass spec
    T.render component {}
