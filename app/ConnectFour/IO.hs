module ConnectFour.IO where

import Lib
import ConnectFour.Definitions
import ConnectFour.Game
import ConnectFour.Board
import ConnectFour.AI as AI

inputToMove :: String -> Maybe Move
inputToMove "A" = Just A
inputToMove "a" = Just A
inputToMove "B" = Just B
inputToMove "b" = Just B
inputToMove "C" = Just C
inputToMove "c" = Just C
inputToMove "D" = Just D
inputToMove "d" = Just D
inputToMove "E" = Just E
inputToMove "e" = Just E
inputToMove "F" = Just F
inputToMove "f" = Just F
inputToMove "G" = Just G
inputToMove "g" = Just G
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

makeAIMove :: Game -> IO ()
makeAIMove game = do
  case (AI.bestMoveAnalysis game 7) of
    (MoveAnalysis move analysis) -> do
      putStrLn ("Computer: " ++ show move)
      putStrLn (show analysis)
      case makeMove game move of
        Left game -> do
          processGame game
        Right error -> putStrLn ("Error: " ++ (show error))

processGame :: Game -> IO ()
processGame game = do
  putStrLn (showGame game)
  case gameState game of
    Active -> do
      case (length game) `mod` 2 of
        0 -> makeAIMove game
        _ -> requestHumanMove game
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
