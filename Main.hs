{-# LANGUAGE OverloadedStrings #-}
import Network.Wai (requestMethod, rawPathInfo, remoteHost, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import System.Process (spawnCommand)

app request respond = case (requestMethod request, rawPathInfo request, getIpFromFullAddress $ show $ remoteHost request) of
    ("GET", "/webhook", "127.0.0.1")    -> goGit request respond
    _                                    -> goAway request respond

getIpFromFullAddress (':':xs) = []
getIpFromFullAddress (x:xs) = x : getIpFromFullAddress xs

goGit request respond = do
    putStrLn $ getIpFromFullAddress $ show $ remoteHost request
    --spawnCommand "git -C /home/simen/src/test.no pull"
    spawnCommand "./runMe.sh"
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
    putStrLn "Running server on 56308"
    run 56308 app
