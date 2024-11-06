module ParserTest (testParser) where

import MathExpr
import Parser
import Result
import Control.Applicative (Alternative (..))
import Data.Char (isAlpha)

testParser :: IO ()
testParser = do
    let str1 = "512321"
        str2 = "2 ^ 2 ^ 2"
        str3 = "9 + 8 - 2"
        str4 = "2 * 3 + 4"
        str5 = "2 + 3 * 4"
        str6 = "(2 + 3 - x) * 4"
        str7 = "x"
        str8 = "2 + x"
        str9 = "(5 - x) * 2"

        space = takeWhileP (== ' ')
        token p = space *> p <* space

        num = token $ EInt . read <$> errLabel "invalid number" <!> takeWhile1P (`elem` "0123456789")
        var = token $ EVar <$> errLabel "invalid varname" <!> takeWhile1P isAlpha

        add = token $ (EAdd <$ exactP '+') <|> (ESub <$ exactP '-')
        mul = token $ (EMul <$ exactP '*') <|> (EDiv <$ exactP '/')
        exp = token (EExp <$ exactP '^')

        atom = token $ num <|> var <|> (exactP '(' *> expr <* exactP ')')

        factor = token $ chainr1P atom exp

        term = token $ chainl1P factor mul

        expr = token $ chainl1P term add

        test = (\res -> maybe "_" show (evalSimple res) ++ " = " ++ show res) <$> expr

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
    print $ runParser test str8
    print $ runParser test str9

    -- print "hi"

    -- print $ runParser parser2 str1
    -- print $ runParser parser2 str2
    -- print $ runParser parser2 str3

    print "hi"