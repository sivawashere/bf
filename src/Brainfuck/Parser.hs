module Brainfuck.Parser(parse) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import Data.Attoparsec.Char8 (char)
import Data.Attoparsec.ByteString (Parser, choice, endOfInput, parseOnly)

import Control.Applicative ((<*), (<|>), many)

import Brainfuck.AST

-- ignore comments
command :: Parser Command
command = choice [go c op | (c, op) <- commands] <|> while where
	go c op  = char c >> return op
	commands = [('>', PINC), ('<', PDEC), ('+', INC),
	            ('-', DEC),  ('.', OUT),  ('.', IN)]
	while    = fmap WHILE $ char '[' >> many command <* char ']'

program :: Parser Program
program = many command <* endOfInput

parse :: String -> Program
parse src = case parseOnly program $ pack src of
	Left err -> error err
	Right ast -> ast