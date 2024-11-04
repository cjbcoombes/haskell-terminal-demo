module ParserTest (testParser) where

import MathExpr
import Parser
import Result
import Control.Applicative (Alternative (..))

testParser :: IO ()
testParser = do
    let str1 = "512321"
        str2 = "5 + 7"
        str3 = "9 + 8 - 2"
        str4 = "2 * 3 + 4"
        str5 = "2 + 3 * 4"
        str6 = "(2 + 3) * 4"
        str7 = ""

        space = takeWhileP (== ' ')
        token p = space *> p <* space

        num = token $ EInt . read <$> errLabel "invalid number" <!> (some1P . predP) (`elem` "0123456789")
        add = token $ const EAdd <$> exactP '+'
        mul = token $ const EMul <$> exactP '*'
        
        factor = token $ num <|> (exactP '(' *> expr <* exactP ')')

        term = token $ chainl1P factor mul

        expr = token $ chainl1P term add

        test = expr
            
    -- let str1 = "123"
    --     str2 = ""
    --     str3 = "!123"
    --     parser1 = maybeP (exactP '!') *> (read <$> takeWhile1P (`elem` "0123456789")) :: Parser Char String Int
    --     parser2 = exactP '!' *> takeWhileP (`elem` "0123456789") :: Parser Char String String

    print $ runParser test str1
    print $ runParser test str2
    print $ runParser test str3
    print $ runParser test str4
    print $ runParser test str5
    print $ runParser test str6
    print $ runParser test str7

    -- print "hi"

    -- print $ runParser parser2 str1
    -- print $ runParser parser2 str2
    -- print $ runParser parser2 str3

    print "hi"