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


-- init :: IO ()
-- init = do
--   inColor <- supportsPretty
--   if inColor then example
--              else putStrLn "Sorry, this terminal doesn't support pretty ANSI codes"

-- example :: IO ()
-- example = do
--   -- simple style
--   putStrLn ( style Underline "hello there!" )

--   -- simple color
--   putStrLn ( color Pretty.Yellow "this lib was designed to be easy" )

--   -- simple background
--   putStrLn ( bgColor Blue "and the functions layer together easily" )

--   -- combining
--   putStrLn ( bgColor White . color Pretty.Red . style Bold $ "like so!" )

--   -- custom style & color
--   let primary = bgColor Magenta . color Green . style Italic
--   putStrLn ( primary "easily create your own helpers & styles" )

--   -- with both unicode string types
--   putStrLn ( color Cyan "String...")
--   T.putStrLn (color Pretty.Red "and Text")

--   -- set styling to none
--   putStrLn ( primary $ style Normal "or if you need to remove any styling..." )

