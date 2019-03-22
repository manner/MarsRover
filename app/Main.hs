module Main where

import Robot
import Control.Monad

main :: IO ()
main = forever $ do
    startPos <- getLine
    -- putStrLn $ show (parsePosition startPos)
    instructions <- getLine
    putStrLn $ show (robot instructions $ parsePosition startPos)

