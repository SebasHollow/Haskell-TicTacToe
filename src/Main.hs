{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Board
import Composer (serialize)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.String
import Text.Printf (printf)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Char8 (unpack)

format :: String
playerID :: Int
turns :: [Square]

encoding = "application/m-expr+list"
format = "http://tictactoe.homedir.eu/game/%s/player/%d"
--gameID = "test-1252346sdfnsfjw7ssasz7"
playerID = 2
turns = [(0, 2, 'o'), (2, 2, 'o')]

main :: IO ()
main = do
    putStrLn "Enter unique game ID: "
    gameID <- getLine
    let url = printf format gameID playerID

    manager <- newManager defaultManagerSettings

    board <- getData manager url

    postData manager url $ board ++ [head turns]
    board <- getData manager url

    postData manager url $ board ++ [last turns]
    board <- getData manager url

    print "Game finished. You lost!"

postData :: Manager -> String -> [Square] -> IO ()
postData manager url board = do
    initialRequest <- parseUrl url
    let request = initialRequest { method = "POST"
                                 , requestBody = (toRequestBody board)
                                 , requestHeaders = [("Content-Type", encoding)]}

    response <- httpLbs request manager
    --putStrLn $ "Status code: " ++ (show . statusCode $ responseStatus response)
    putStrLn $ "Sent:    " ++ show board

getData :: Manager -> String -> IO [Square]
getData manager url = do
    request <- parseUrl url
    let request' = request { method = "GET"
                           , requestHeaders = [("Accept", encoding)] }
    response <- httpLbs request' manager

    --putStrLn $ "Status code: " ++ (show . statusCode $ responseStatus response)
    let board = parse $ unpack $ toStrict (responseBody response)
    putStrLn $ "Received " ++ show board
    return board

toRequestBody :: [Square] -> RequestBody
toRequestBody string = RequestBodyBS . fromString $ serialize string


{-httpTest :: IO ()
httpTest = do
    putStrLn "Enter unique game ID: "
    gameID <- getLine
    let url = printf format gameID (1 :: Int)
    let url2 = printf format gameID (2 :: Int)

    manager <- newManager defaultManagerSettings
    manager2 <- newManager defaultManagerSettings

    postData manager2 url2 [(0, 1, 'x')]

    board <- getData manager url
    postData manager url $ board ++ [head turns]

    board <- getData manager2 url2
    postData manager2 url2 $ board ++ [(1, 1, 'x')]

    board <- getData manager url
    postData manager url $ board ++ [last turns]

    board <- getData manager2 url2
    postData manager2 url2 $ board ++ [(2, 1, 'x')]

    board <- getData manager url
    print "Game finished. You lost!"-}
