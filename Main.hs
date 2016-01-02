{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class
import System.Process

app request respond = case (requestMethod request, rawPathInfo request, getIpFromFullAdress $ show $ remoteHost request) of
    ("GET", "/webhook", "127.0.0.1")    -> goGit request respond
    _                                    -> goAway request respond

getIpFromFullAdress (':':xs) = []
getIpFromFullAdress (x:xs) = x : getIpFromFullAdress xs

goGit request respond = do
    putStrLn $ show $ remoteHost request
    spawnCommand "git -C /home/simen/src/test.no pull"
    respond $ makePage "Fine!"

goAway request respond = do 
    putStrLn $ show $ remoteHost request
    respond $ makePage "GO AWAY!"

makePage content = responseLBS
    status200
    [("Content-Type", "text/plain")]
    content
    

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
