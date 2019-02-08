module Main where

import qualified Servant as S
import Servant ((:<|>), (:>))
import qualified Servant.Server as SS
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Network.Wai.Servlet.Handler.Jetty as J

type API = "fibonacci" :> S.Capture "n" Int :> S.Get '[S.JSON] A.Value
      :<|> "backwards" :> S.Capture "s" String :> S.Get '[S.JSON] A.Value

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

doReverse :: String -> String
doReverse s = reverse s

api :: S.Proxy API
api = S.Proxy

server :: SS.Server API
server = fibHandler S.:<|> revHandler
  where fibHandler n
          | n >= 0
          = return $ A.object ["n" .= n, "fib" .= fib n]
          | otherwise = S.throwError $ SS.err412 { SS.errBody = "Precondition Failed: n >= 0" }
        revHandler s = return $ A.object ["original" .= s, "backwards" .= (reverse s)]

app :: SS.Application
app = SS.serve api server

main :: IO ()
main = J.run 9000 app
