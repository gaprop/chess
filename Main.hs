module Main where
import Chess
import Chess.Control
import System.Console.ANSI
import System.IO

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  controlCursor
  main

controlCursor = do
  c <- getChar
  clearScreen
  putStrLn "Hello"
  case c of
    'h' -> cursorBackward 2
    'j' -> do 
      cursorDownLine 1
      cursorBackward 1
    'k' -> do 
      cursorUpLine 1
      cursorBackward 1
    'l' -> do
      cursorForward 1 
      cursorBackward 1
    _ -> putStrLn $ [c]
