{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Parser
import Board
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Conduit

httpTestMsg = "http://tictactoe.homedir.eu/game/test1/player/1"

msg1 = "l[m[\"x\"; 2; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"x\"]]"
msg2 = "l[m[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 2; \"y\"; 1; \"v\"; \"x\"]]"
msg3 = "l[m[\"x\"; 1; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 2; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"x\"]]"
msg4 = "l[m[\"x\"; 0; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"o\"]; m[\"x\"; 2; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"o\"]; m[\"x\"; 2; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]]"
msg5 = "l[m[\"x\"; 2; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"o\"]]"

move :: String -> (Maybe (Int, Int, Char))
move msg = getNextTurn turns
    where turns = parse msg

main :: IO ()
main = do
    response <- simpleHttp httpTestMsg
    return ()