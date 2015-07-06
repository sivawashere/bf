{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Brainfuck.Interpreter(run) where

import Data.Word (Word8)

import qualified Data.Vector as V
import Data.Vector.Mutable (write)

import Control.Monad (mapM_, when)
import Control.Monad.State (StateT, modify, gets, lift, evalStateT)

import Data.ByteString.Internal (w2c, c2w)

import Brainfuck.AST

data VM = VM { ptr :: Int, mem :: V.Vector Word8 } deriving Show

type Brainfuck = StateT VM IO

modifyPtr :: (Int -> Int) -> Brainfuck ()
modifyPtr f = modify $ \vm@VM{..} -> vm { ptr = f ptr }

modifyByte :: (Word8 -> Word8) -> Brainfuck ()
modifyByte f = modify $ \vm@VM{..} ->
	vm { mem = V.modify (\v -> V.indexM mem ptr >>= write v ptr . f) mem }

writeByte :: Word8 -> Brainfuck ()
writeByte = modifyByte . const

readByte :: Brainfuck Word8
readByte = gets $ \VM{..} -> mem V.! ptr

exec :: Program -> Brainfuck ()
exec = mapM_ $ \case
	PINC -> modifyPtr succ
	PDEC -> modifyPtr pred
	INC  -> modifyByte succ
	DEC  -> modifyByte pred
	OUT  -> fmap w2c readByte >>= lift . print
	IN   -> lift (fmap c2w getChar) >>= writeByte
	WHILE block -> readByte >>= \d -> when (d /= 0) $ exec block

run :: Program -> IO ()
run = (flip evalStateT) (VM 0 $ V.replicate 30000 0) . exec