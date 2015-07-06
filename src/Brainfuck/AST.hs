module Brainfuck.AST where

data Command = PINC | PDEC | INC | DEC | OUT | IN | WHILE [Command] deriving Show

type Program = [Command]