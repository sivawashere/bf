module Main where

import System.Environment (getArgs)

import Brainfuck.Parser
import Brainfuck.Interpreter

main :: IO ()
main = fmap head getArgs >>= fmap parse . readFile >>= run