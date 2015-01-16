module UI where

import Control.Monad.Eff
import Data.Either

import Data.JSON
import Data.Function
import Data.Map
import Data.Maybe
import Data.Traversable
import Data.Tuple

import Control.Monad.Eff.AJAX

import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.Cont.Trans
import Control.Monad.Trans

import Data.JSON

import qualified Network.Oboe as O

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import Debug.Trace
import Debug.Foreign

-- | Depicts all the actions taking place in our app.
data Action = TryAuthenticate String String | LogOut

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

-- | Marshalls AuthPack to JSON
instance authPackToJSON :: ToJSON AuthPack where
    toJSON (AuthPack ap) =
        object [ "username" .= ap.username
               , "password" .= ap.password ]

--------------------------------------------------------------------------------
-- | Determines whether login attempt was successful or not.
data AuthResult = Authenticated {
                    identity :: String,
                    token    :: String,
                    expiry   :: String }
                | Unauthenticated

-- | Shows AuthResult
instance showAuthResult :: Show AuthResult where
    show (Authenticated a) = "Authenticated " ++ a.identity ++ ", " ++ a.token ++ ", " ++ a.expiry
    show Unauthenticated   = "Unauthenticated"

-- | Marshalls AuthResult from JSON
instance authResultFromJson :: FromJSON AuthResult where
    parseJSON (JObject o) = do
        success <- o .: "success"
        case success of
            true -> do
                identity <- o .: "identity"
                token    <- o .: "token"
                expiry   <- o .: "expiry"
                return $ Authenticated {identity: identity, token: token, expiry: expiry}
            _ -> return $ Unauthenticated
    parseJSON _ = fail "AuthResult parse failed."

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
    loggedIn = T.p' [ T.text $ show s.authToken
                    , T.text "You're logged in!"
                    , T.button [ T.onClick ctx (\_ -> LogOut) ]
                               [ T.text "Log Out" ] ]
    loginForm = T.div' [
                  T.p' [ T.text $ show s.authToken ]
                  , fieldGroup [
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
                                 , T.onClick ctx handleSubmit ]
                                 [ T.text "Log In" ]
                  ] ]
    fieldGroup children = T.div [ A.className "form-group" ] children

handleSubmit :: T.MouseEvent -> Action
handleSubmit _ =
    case Tuple eav pwv of
        (Tuple (Just u) (Just p)) -> TryAuthenticate u p
        _ -> LogOut
  where
    eav = getSelectorValue "#email-address"
    pwv = getSelectorValue "#password"

--------------------------------------------------------------------------------
-- | Modifies the global app state based on the action taking place
performAction :: T.PerformAction _ Action (T.Action _ State) 
performAction _ (TryAuthenticate u p) = auth $
    (AuthPack {username: u, password: p})
performAction _ LogOut      = T.setState { authToken: Nothing :: Maybe String }

auth :: AuthPack -> T.Action _ State Unit
auth ap = do
    let headers = [ Tuple "Content-Type" "application/json;charset=UTF-8"
                  , Tuple "Accept" "application/json" ]
    json <- T.async $ post "http://localhost:8080/auth" (encode ap) headers
    let y = fprintUnsafe $ show (decode json :: Maybe AuthResult)
    case (decode json :: Maybe AuthResult) of
        Just (Authenticated a) -> T.setState $
            { authToken: Just a.token :: Maybe String }
        _                      -> T.setState $
            { authToken: Nothing :: Maybe String }

--------------------------------------------------------------------------------
spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount LogOut

main = do
    let component = T.createClass spec
    T.render component {}
