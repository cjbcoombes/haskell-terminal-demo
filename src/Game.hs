module Game (playGame) where


-- import System.IO (hSetEcho, hFlush, hReady, stdin, stdout)
import System.Console.ANSI
import MathExpr
import Parser (ParseError, runParser, eofP)
import Result

box :: Int -> Int -> String
box w h = "┌" ++ bar ++ "┐\n" ++ rows ++ "└" ++ bar ++ "┘\n"
    where bar = replicate w '─'
          space = replicate w ' '
          row = "│" ++ space ++ "│\n"
          rows = concat (replicate h row)

width :: Int
width = 100
height :: Int
height = 20

parseInput s = mapReject (const "error") $ fst <$> runParser (exprParser <* eofP) s


inputLoop :: Int -> IO ()
inputLoop x | x >= height - 1 = return ()
            | otherwise = do
                setCursorPosition height 1
                putStr $ ">" ++ replicate (width - 1) ' '
                setCursorPosition height 3
                line <- take width <$> getLine
                setCursorPosition (x + 1) 1
                putStr line
                inputLoop (x + 1)

playGame :: IO ()
playGame = do
    clearScreen
    setCursorPosition 0 0
    putStr (box width height)
    inputLoop 0
    setCursorPosition (height + 2) 0