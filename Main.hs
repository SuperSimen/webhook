{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B
import System.Process
import qualified Data.Text.Lazy as T

hello _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"



app3 :: Application
app3 request respond = case rawPathInfo request of
    _       -> onlyPost request respond

onlyPost request respond = case (requestMethod request, rawPathInfo request, show $ remoteHost request) of
    ("POST", "/BOI", '1':'2':'7':'.':'0':'.':'0':'.':'1':x)  -> blast request respond
    _       -> goAway request respond


        
blast request respond = do
    putStrLn $ show $ remoteHost request
    beam <- liftIO (readProcess "echo" ["oh yeah"] "")
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        $ do
            mconcat [B.pack beam, "yeah", B.pack $ show $ requestMethod request]

goAway request respond = respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "GO AWAY"


main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app3
