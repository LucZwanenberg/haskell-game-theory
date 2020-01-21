module ConnectFour.IO where

import Lib
import ConnectFour.Definitions
import ConnectFour.Game
import ConnectFour.Board

inputToMove :: String -> Maybe Move
inputToMove "A" = Just A
inputToMove "B" = Just B
inputToMove "C" = Just C
inputToMove "D" = Just D
inputToMove "E" = Just E
inputToMove "F" = Just F
inputToMove "G" = Just G
inputToMove _ = Nothing

requestHumanMove :: Game -> IO ()
requestHumanMove game = do
  putStrLn "Enter your move:"
  input <- getLine
  case inputToMove input of
    Nothing -> do
      putStrLn "Unknown move"
      requestHumanMove game
    Just move -> processHumanMove game move

processHumanMove :: Game -> Move -> IO ()
processHumanMove game move = do
  case makeMove game move of
    Left game -> processGame game
    Right error -> do
      putStrLn ( "Error: " ++ (show error) )
      requestHumanMove game

processGame :: Game -> IO ()
processGame game = do
  putStrLn (showGame game)
  case gameState game of
    Active -> do
      case (length game) `mod` 2 of
        0 -> requestHumanMove game
        _ -> requestHumanMove game -- TODO: make AI move
    a -> do
      putStrLn (show a)
      requestRematch ()

requestRematch :: () -> IO ()
requestRematch _ = do
  putStrLn "Rematch? (y/n)"
  input <- getLine
  case input of
    "y" -> processGame []
    _ -> return ()

init :: IO ()
init = processGame []
