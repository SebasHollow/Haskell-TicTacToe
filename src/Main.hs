{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Board (getTurn, Square)
import Composer (serialize)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.String (fromString)
import Text.Printf (printf)

testMsg = "l[m[\"x\"; 2; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"o\"]]"
testMsg' = "l[m[\"x\"; 2; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"o\"]]"

format :: String
gameID :: String
playerID :: Int

format = "http://tictactoe.homedir.eu/game/%s/player/%d"
gameID = "test-1231234-51525-51"
playerID = 1

main :: IO ()
main = postData

requestBody' = RequestBodyBS $ fromString $ serialize testMsg'

postData = do
    manager <- newManager defaultManagerSettings

    initialRequest <- parseUrl $ printf format gameID (1 :: Int)
    let request = initialRequest { method = "POST", requestBody = requestBody', requestHeaders = [("Content-Type", "application/m-expr+list")] }

    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response

getData = do
    manager <- newManager defaultManagerSettings

    request <- parseUrl $ printf format gameID (2 :: Int)
    let request' = request { method = "GET", requestHeaders = [("Accept", "application/m-expr+list")] }
    response <- httpLbs request' manager

    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response

