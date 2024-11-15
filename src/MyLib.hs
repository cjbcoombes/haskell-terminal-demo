module MyLib (someFunc) where

import System.IO (hSetEcho, hFlush, hReady, stdin, stdout)
import System.Console.ANSI
import Control.Monad (forM_)
import Game (playGame)

someFunc :: IO ()
someFunc = do
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    if stdoutSupportsANSI
    then do
        playGame
    else
        putStrLn "Standard output does not support 'ANSI' escape codes."