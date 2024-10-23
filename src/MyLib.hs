module MyLib (someFunc) where

import System.IO (hFlush, stdout)
import System.Console.ANSI

someFunc :: IO ()
someFunc = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
      clearScreen
      setCursorPosition 0 0
      setSGR [SetColor Foreground Dull Blue]
      putStr "Enter your name: "
      setSGR [SetColor Foreground Dull Yellow]
      hFlush stdout  -- flush the output buffer before getLine
      name <- getLine
      setSGR [SetColor Foreground Dull Blue]
      putStrLn $ "Hello, " ++ name ++ "!"
      setSGR [Reset]  -- reset to default colour scheme
    else
      putStrLn "Standard output does not support 'ANSI' escape codes."
