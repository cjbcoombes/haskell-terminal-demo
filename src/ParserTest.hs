module ParserTest (testParser) where

import MathExpr
import Parser
import Result
import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import Data.Map (Map, empty, insert, foldrWithKey, mapWithKey)
import Data.Maybe (fromMaybe)

testParser :: IO ()
testParser = do
    let str1 = "512321"
        str2 = "2 ^ 2 ^ 2"
        str3 = "9 + 8 - 2 where x = 2"
        str4 = "2 * 3 + 4"
        str5 = "2 + 3 * 4"
        str6 = "(2 + 3 - x) * 4 where x = 4"
        str7 = "x * y + z where x = 1, y = 2 * x, z = 3"
        str8 = "2 + x"
        str9 = "(5 - x) * 2"

        space = takeWhileP (== ' ')
        token p = space *> p <* space

        num = token $ EInt . read <$> errLabel "invalid number" <!> takeWhile1P (`elem` "0123456789")
        rawvar = errLabel "invalid varname" <!> takeWhile1P isAlpha
        var = token $ EVar <$> rawvar

        add = token $ (EAdd <$ exactP '+') <|> (ESub <$ exactP '-')
        mul = token $ (EMul <$ exactP '*') <|> (EDiv <$ exactP '/')
        exp = token (EExp <$ exactP '^')
        eq = token (exactP '=')
        com = token (exactP ',')
        where' = token $ stringP "where"

        assgn = (,) <$> rawvar <* eq <*> arith
        mkmap = foldr (uncurry insert) empty
        clause = mkmap <$> (where' *> chainr1P ((:[]) <$> assgn) ((++) <$ com))

        atom = num <|> var <|> (exactP '(' *> expr <* exactP ')')

        factor = chainr1P atom exp

        term = chainl1P factor mul

        arith = chainl1P term add

        expr = EWhere <$> arith <*> (fromMaybe empty <$> maybeP clause)

        present res = maybe "_" show (eval res) ++ " = " ++ show res

        test = present <$> expr

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