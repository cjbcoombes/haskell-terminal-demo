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
import System.Random (randomIO)

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
width = 100
sidewidth :: Int
sidewidth = 21
height :: Int
height = 21

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
scoreExpr (EVar v) = 3
scoreExpr (ENeg e) = scoreExpr e
scoreExpr (EAdd a b) = scoreExpr a + scoreExpr b + 1
scoreExpr (EMul a b) = scoreExpr a + scoreExpr b + 4
scoreExpr (ESub a b) = scoreExpr a + scoreExpr b + 1
scoreExpr (EDiv a b) = scoreExpr a + scoreExpr b + 2
scoreExpr (EExp a b) = scoreExpr a + scoreExpr b + 7
scoreExpr (EWhere a m) = scoreExpr a + foldr ((+) . scoreExpr) 0 m

colorExpr :: Expr -> IO ()
colorExpr x = write x >> setSGR [Reset]
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

colorEval :: Int -> Expr -> IO ()
colorEval tgt e = maybe wrong (draw . fromIntegral) (eval e) >> setSGR [Reset]
    where wrong = printColor Red "(!)"
          draw x | x > 1000 = printColor Red ">1000"
                 | x < 0 = printColor Red "< 0"
                 | x == tgt = printColor Green (show x)
                 | otherwise = printColor Red (show x)

inputLoop :: Int -> Int -> IO ()
inputLoop tgt x | x >= height - 1 = return ()
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
                            Accept e -> colorEval tgt e
                            Reject e -> printColor Red "(!)"
                        setSGR [Reset]
                        setCursorPos $ (+ x) <$> scorePos
                        case expr of
                            Accept e -> printColor Magenta (show $ scoreExpr e)
                            Reject e -> printColor Red "(!)"
                        setSGR [Reset]
                        hFlush stdout
                        inputLoop tgt (x + 1))

playGame :: IO ()
playGame = do
    clearScreen
    setCursorPos (0, 0)
    putStr (splitbox [width, 6, 6, sidewidth] height)

    rand <- randomIO :: IO Int
    let target = 20 + rand `mod` 980

    setCursorPos $ (+1) <$> instrPos
    putStr "    Target: " >> printColor Green (show target) >> setSGR [Reset]
    setCursorPos $ (+3) <$> instrPos
    putStr " Write an expression"
    setCursorPos $ (+4) <$> instrPos
    putStr " that evaluates to"
    setCursorPos $ (+5) <$> instrPos
    putStr " the given target."
    setCursorPos $ (+6) <$> instrPos
    putStr " Minimize cost."
    setCursorPos $ (+8) <$> instrPos
    printColor Blue "    where" >> printColor White " = " >> printColor Green "free"
    setCursorPos $ (+9) <$> instrPos
    printColor Yellow "       ()" >> printColor White " = " >> printColor Green "free"
    setCursorPos $ (+10) <$> instrPos
    printColor Cyan "        #" >> printColor White " = " >> printColor Yellow "value"
    setCursorPos $ (+11) <$> instrPos
    printColor Red "        v" >> printColor White " = " >> printColor Red "3"
    setCursorPos $ (+12) <$> instrPos
    printColor White "        + = " >> printColor Red "1"
    setCursorPos $ (+13) <$> instrPos
    printColor White "        - = " >> printColor Red "1"
    setCursorPos $ (+14) <$> instrPos
    printColor White "        / = " >> printColor Red "2"
    setCursorPos $ (+15) <$> instrPos
    printColor White "        * = " >> printColor Red "4"
    setCursorPos $ (+16) <$> instrPos
    printColor White "        ^ = " >> printColor Red "7"
    setCursorPos $ (+18) <$> instrPos
    printColor White " Ex:"
    setCursorPos $ (+19) <$> instrPos
    printColor White " -" >> printColor Yellow "(" >> printColor Red "x" >> printColor White " - " 
        >> printColor Cyan "7" >> printColor Yellow ")" >> printColor Blue " where " >> printColor Red "x" 
        >> printColor White "=" >> printColor Cyan "2"
    setSGR [Reset]

    inputLoop target 0
    setCursorPos quitPos

--                    |
--    Target=[...]
--
--Write an expression
--that evaluates to
--the given target.
--Minimize cost.
--
--    where = free
--       () = free
--        # = value
--        v = 3
--        + = 1      
--        - = 1
--        / = 2
--        * = 4
--        ^ = 7