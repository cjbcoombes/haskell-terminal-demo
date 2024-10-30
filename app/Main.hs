module Main where

import qualified MyLib (someFunc)
import qualified ParserTest (testParser)

main :: IO ()
main = do
  -- MyLib.someFunc
  ParserTest.testParser
