module Game (playGame) where


import System.Console.ANSI
import MathExpr
import Parser (ParseError, runParser, eofP)
import Data.Map (foldrWithKey)
import Result
import Control.Applicative (liftA2)
import Control.Monad (void, unless)
import System.IO (stdout, hFlush)
import Data.List (intercalate)

box :: Int -> Int -> String
box w h = "┌" ++ bar ++ "┐\n" ++ rows ++ "└" ++ bar ++ "┘\n"
    where bar = replicate w '─'
          space = replicate w ' '
          row = "│" ++ space ++ "│\n"
          rows = concat (replicate h row)

splitbox :: [Int] -> Int -> String
splitbox w h = "┌" ++ divide "┬" '─' ++ "┐\n" ++ rows ++ "└" ++ divide "┴" '─' ++ "┘\n"
    where divide c f = intercalate c (map (`replicate` f) w)
          row = "│" ++ divide "│" ' ' ++ "│\n"
          rows = concat (replicate h row)

width :: Int
width = 70
sidewidth :: Int
sidewidth = 20
height :: Int
height = 20

outputPos :: (Int, Int)
outputPos = (1, 1)
cursorPos :: (Int, Int)
cursorPos = (1, height)
typingPos :: (Int, Int)
typingPos = (3, height)
instrPos :: (Int, Int)
instrPos = (width + 16, 1)
evalPos :: (Int, Int)
evalPos = (width + 3, 1)
scorePos :: (Int, Int)
scorePos = (width + 10, 1)
quitPos :: (Int, Int)
quitPos = (0, height + 2)

setCursorPos :: (Int, Int) -> IO ()
setCursorPos = uncurry $ flip setCursorPosition

printColor :: Color -> String -> IO ()
printColor c s = setSGR [SetColor Foreground Vivid c] >> putStr s 

parseInput :: String -> Result String Expr
parseInput s = mapReject (const "error") $ fst <$> runParser (exprParser <* eofP) s

scoreExpr :: Expr -> Int
scoreExpr (EInt i) = fromIntegral i
scoreExpr (EVar v) = 5
scoreExpr (ENeg e) = scoreExpr e
scoreExpr (EAdd a b) = scoreExpr a + scoreExpr b + 1
scoreExpr (EMul a b) = scoreExpr a + scoreExpr b + 4
scoreExpr (ESub a b) = scoreExpr a + scoreExpr b + 1
scoreExpr (EDiv a b) = scoreExpr a + scoreExpr b + 2
scoreExpr (EExp a b) = scoreExpr a + scoreExpr b + 7
scoreExpr (EWhere a m) = scoreExpr a + foldr ((+) . scoreExpr) 0 m

colorExpr :: Expr -> IO ()
colorExpr x = write x >> setSGR [Reset] >> putStrLn ""
    where
        write (EInt i) = printColor icol $ show i
        write (EVar v) = printColor vcol v
        write (ENeg e) = (opcol, "-") <++ write e
        write (EAdd a b) = (pcol, "(") <++ write a ++> (opcol, " + ") >> write b ++> (pcol, ")")
        write (EMul a b) = (pcol, "(") <++ write a ++> (opcol, " * ") >> write b ++> (pcol, ")")
        write (ESub a b) = (pcol, "(") <++ write a ++> (opcol, " - ") >> write b ++> (pcol, ")")
        write (EDiv a b) = (pcol, "(") <++ write a ++> (opcol, " / ") >> write b ++> (pcol, ")")
        write (EExp a b) = (pcol, "(") <++ write a ++> (opcol, " ^ ") >> write b ++> (pcol, ")")
        write (EWhere a m) | null m = write a
                           | otherwise = write a ++> (wcol, " where ") >> writekv (foldrWithKey (\k v a -> (k,v):a) [] m)

        writekv [] = return ()
        writekv [(k,v)] = (vcol, k) <++ ((opcol, " = ") <++ write v)
        writekv ((k,v):xs) = (vcol, k) <++ ((opcol, " = ") <++ write v) ++> (comcol, ", ") >> writekv xs

        (<++) (c, a) b = printColor c a >> b
        (++>) a (c, b) = a >> printColor c b

        opcol = White
        pcol = Yellow
        icol = Cyan
        vcol = Red
        wcol = Blue
        comcol = White

colorEval :: Expr -> IO ()
colorEval e = maybe wrong draw (eval e) >> setSGR [Reset]
    where wrong = printColor Red "(!)"
          draw x | x > 1000 = printColor Red ">1000"
                 | x < 0 = printColor Red "< 0"
                 | otherwise = printColor Green (show x)

inputLoop :: Int -> IO ()
inputLoop x | x >= height - 1 = return ()
            | otherwise = do
                setCursorPos cursorPos
                putStr $ ">" ++ replicate (width - 1) ' '
                setCursorPos typingPos
                hFlush stdout
                line <- take width <$> getLine
                unless (line == "quit") (do
                    let expr = parseInput line
                    setCursorPos $ (+ x) <$> outputPos
                    case expr of
                        Accept e -> colorExpr e
                        Reject e -> printColor Red "(!)"
                    setSGR [Reset]
                    setCursorPos $ (+ x) <$> evalPos
                    case expr of
                        Accept e -> colorEval e
                        Reject e -> printColor Red "(!)"
                    setSGR [Reset]
                    setCursorPos $ (+ x) <$> scorePos
                    case expr of
                        Accept e -> printColor Magenta (show $ scoreExpr e)
                        Reject e -> printColor Red "(!)"
                    setSGR [Reset]
                    hFlush stdout
                    inputLoop (x + 1))

playGame :: IO ()
playGame = do
    clearScreen
    setCursorPos (0, 0)
    putStr (splitbox [width, 6, 6, sidewidth] height)
    inputLoop 0
    setCursorPos quitPos
