module ConnectFour.PrettyBoard where

import qualified Data.Text.IO as T
import System.Console.Pretty as Pretty

import ConnectFour.Game
import ConnectFour.Definitions

header = " A  B  C  D  E  F  G\n"

showSlot :: Slot -> [Char]
showSlot Nothing = "[ ]"
showSlot (Just ConnectFour.Definitions.Yellow) = (color Pretty.Yellow "[X]")
showSlot (Just ConnectFour.Definitions.Red) = (color Pretty.Red "[O]")
showRow (a0, a1, a2, a3, a4, a5, a6) = (showSlot a0) ++ "" ++ (showSlot a1) ++ "" ++ (showSlot a2) ++ "" ++ (showSlot a3) ++ "" ++ (showSlot a4) ++ "" ++ (showSlot a5) ++ "" ++ (showSlot a6)
showBoard (a0, a1, a2, a3, a4, a5) = header ++ (showRow a0) ++ "\n" ++ (showRow a1) ++ "\n" ++ (showRow a2) ++ "\n" ++ (showRow a3) ++ "\n" ++ (showRow a4) ++ "\n" ++ (showRow a5)

prettyPrintGame :: Game -> IO ()
prettyPrintGame game = do
  let board = gameToBoard game in
    putStrLn ( showBoard board )
