module Control.Monad.Eff.AJAX where
  
import Control.Monad.Eff
import Data.Map
import Data.Tuple

foreign import data XHR :: !

type URI = String
type HttpHeaders = [Tuple String String]

-- | Send a HTTP GET
foreign import get 
  "function get(uri) {\
  \  return function(k) {\
  \    return function() {\
  \      var req = new XMLHttpRequest();\
  \      req.onreadystatechange = function() {\
  \        if (req.readyState === 4 && req.status === 200) {\
  \          k(req.responseText)();\
  \        }\
  \      };\
  \      req.open('GET', uri, true);\
  \      req.send();\
  \    };\
  \  };\
  \}" :: forall eff
      . URI -- ^ URI to GET
      -> (String -> Eff (xhr :: XHR | eff) Unit) -- ^ Callback method to run
      -> Eff (xhr :: XHR | eff) Unit

-- | Send a HTTP POST
foreign import post
  """
  function post(uri) {
    return function(body) {
      return function(headers) {
        return function(k) {
          return function(){
            var req = new XMLHttpRequest();
            var reqOK = function(){
              return req.status >= 200 && req.status < 300;
            };
            req.onreadystatechange = function() {
              if (req.readyState === 4 && reqOK()) {
                k(req.responseText)();
              }
            };
            req.open('POST', uri, true);
            for (var i in headers) {
              req.setRequestHeader(headers[i].value0, headers[i].value1);
            }
            req.send(body);
          };
        };
      };
    };
  }
  """ :: forall eff
      . URI -- ^ URI to POST to
      -> String -- ^ Body to send
      -> HttpHeaders -- ^ HTTP headers to send
      -> (String -> Eff (xhr :: XHR | eff) Unit) -- ^ Callback method to run
      -> Eff (xhr :: XHR | eff) Unit
