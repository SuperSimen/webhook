{-# LANGUAGE OverloadedStrings #-}
import Network.Wai (requestMethod, rawPathInfo, remoteHost, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import System.Process (spawnCommand)
import Data.IP (IPv4, AddrRange, isMatchedTo)

filterRequest request respond = case (requestMethod request, rawPathInfo request, checkRanges request) of
    ("POST", "/webhook", True)    -> runTheCommand request respond
    blast@_                                    -> goAway request respond blast

getIpFromFullAddress (':':xs) = []
getIpFromFullAddress (x:xs) = x : getIpFromFullAddress xs

getIpFromRequest request = getIpFromFullAddress $ show $ remoteHost request

app request respond = do
    putStrLn $ show $ remoteHost request
    putStrLn $ getIpFromRequest request
    filterRequest request respond

inIpRange ip range = isMatchedTo (read ip :: IPv4) (read range :: AddrRange IPv4)

checkRanges request
    | inIpRange ip "127.0.0.1/24" = True
    | inIpRange ip "131.103.20.160/27" = True
    | inIpRange ip "165.254.145.0/26" = True
    | inIpRange ip "104.192.143.0/24" = True
    | otherwise = False
        where ip = getIpFromRequest request

runTheCommand request respond = do
    putStrLn "Valid request"
    spawnCommand "./runMe.sh"
    respond $ makePage "Fine!"

goAway request respond blast = do 
    putStrLn "Invalid request"
    putStrLn $ show $ blast
    respond $ makePage "GO AWAY!"

makePage content = responseLBS
    status200
    [("Content-Type", "text/plain")]
    content
    

main :: IO ()
main = do
    putStrLn $ show "Running on 56308"
    run 56308 app
