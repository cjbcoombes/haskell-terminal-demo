module MyLib (someFunc) where

import System.IO (hSetBuffering, hSetEcho, hFlush, hReady, BufferMode(NoBuffering), stdin, stdout)
import System.Console.ANSI
import Control.Monad (forM_)

box = "┌─────┐\n│     │\n│     │\n│     │\n│     │\n│     │\n└─────┘"

someFunc :: IO ()
someFunc = do
  hSetBuffering stdin NoBuffering

  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
      clearScreen
      setCursorPosition 0 0
      putStr box
      
      forM_ [1..5] (\i -> setCursorPosition i i >> putStr "*")
      foldr (\i a -> a >> setCursorPosition i (6 - i) >> putStr "*") (return ()) [1..5]


      setCursorPosition 10 0
      hFlush stdout
    else
      putStrLn "Standard output does not support 'ANSI' escape codes."