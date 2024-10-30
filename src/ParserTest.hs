module ParserTest (testParser) where

import MathExpr
import Parser

testParser :: IO ()
testParser = do
    let str1 = "123"
        str2 = ""
        str3 = "234"
        parser = takeWhileP (`elem` "0123456789") :: Parser Char String String
    
    print $ runParser parser str1
    print $ runParser parser str2
    print $ runParser parser str3

    print "hi"