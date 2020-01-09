module Main where

import Lib
import Game
import Board

inputToMove :: String -> Maybe Move
inputToMove "A1" = Just ( Move A One )
inputToMove "A2" = Just ( Move A Two )
inputToMove "A3" = Just ( Move A Three )
inputToMove "B1" = Just ( Move B One )
inputToMove "B2" = Just ( Move B Two )
inputToMove "B3" = Just ( Move B Three )
inputToMove "C1" = Just ( Move C One )
inputToMove "C2" = Just ( Move C Two )
inputToMove "C3" = Just ( Move C Three )
inputToMove _ = Nothing

requestMove :: Game -> IO ()
requestMove game = do
  putStrLn (show (deriveBoard game))
  putStrLn "Enter your move:"
  input <- getLine
  case inputToMove input of
    Nothing -> do
      putStrLn "Unknown move."
      Main.requestMove game
    Just move -> processMove game move

processMove :: Game -> Move -> IO ()
processMove game move = do
  case Game.makeMove game move of
    Left game -> requestMove game
    Right error -> do
      putStrLn ( "Error: " ++ (show error) )
      requestMove game

processGame :: Game -> IO ()
processGame game = do
  case gameState game of
    Active -> requestMove game
    a -> putStrLn (show a)

main :: IO ()
main = processGame []

