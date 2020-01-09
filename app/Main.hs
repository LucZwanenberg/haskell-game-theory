module Main where

import Lib
import Game
import Board
import AI

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

requestHumanMove :: Game -> IO ()
requestHumanMove game = do
  putStrLn "Enter your move:"
  input <- getLine
  case inputToMove input of
    Nothing -> do
      putStrLn "Unknown move"
      Main.requestHumanMove game
    Just move -> processHumanMove game move

processHumanMove :: Game -> Move -> IO ()
processHumanMove game move = do
  case Game.makeMove game move of
    Left game -> processGame game
    Right error -> do
      putStrLn ( "Error: " ++ (show error) )
      requestHumanMove game

makeAIMove :: Game -> IO ()
makeAIMove game = do
  let move = AI.getMove game in do
    putStrLn ("Computer: " ++ show move)
    case Game.makeMove game move of
      Left game -> do
        processGame game
      Right error -> putStrLn ("Error: " ++ (show error))

processGame :: Game -> IO ()
processGame game = do
  putStrLn (show (deriveBoard game))
  case gameState game of
    Active -> do
      case (length game) `mod` 2 of
        0 -> requestHumanMove game
        _ -> makeAIMove game
    a -> do
      putStrLn (show a)
      requestRematch ()

requestRematch :: () -> IO ()
requestRematch _ = do
  putStrLn "Rematch? (y/n)"
  input <- getLine
  case input of
    "y" -> Main.processGame []
    _ -> return ()

main :: IO ()
main = processGame []

