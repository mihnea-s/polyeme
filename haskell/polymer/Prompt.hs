module Prompt where


import System.Console.ANSI

-- import Paths_polyeme (version)
-- import Data.Version (showVersion)

showVersion = id
version = "0.0.1"


putWelcomeMsg :: IO ()
putWelcomeMsg = do
  setSGR [SetColor Foreground Vivid Red]
  putStr "Polymer"

  setSGR [Reset]
  putStr " / "

  setSGR [SetColor Foreground Vivid Red]
  putStrLn "Polyeme"

  setSGR [Reset]
  putStr "Version "

  setSGR [SetColor Foreground Vivid Green]
  putStr $ showVersion version

  setSGR
    [ SetColor Foreground Vivid Magenta,
      SetConsoleIntensity BoldIntensity
    ]
  putStrLn " Haskell."

  setSGR [Reset]
  putStr "Press "

  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "Ctrl-C"

  setSGR [Reset]
  putStrLn " to quit."
  putStrLn ""

promptIdle :: String
promptIdle =
  setSGRCode [SetColor Foreground Dull Black, SetConsoleIntensity BoldIntensity]
    ++ "po"
    ++ setSGRCode [Reset, SetConsoleIntensity BoldIntensity]
    ++ " >> "
    ++ setSGRCode [Reset]

promptOk :: String
promptOk =
  setSGRCode [SetColor Foreground Dull Blue, SetConsoleIntensity BoldIntensity]
    ++ "po"
    ++ setSGRCode [Reset, SetConsoleIntensity BoldIntensity]
    ++ " >> "
    ++ setSGRCode [Reset]

promptErr :: String
promptErr =
  setSGRCode [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity]
    ++ "po"
    ++ setSGRCode [Reset, SetConsoleIntensity BoldIntensity]
    ++ " >> "
    ++ setSGRCode [Reset]