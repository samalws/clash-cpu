module CPU.Types (Nybble, Byte, TwoByte, Word, SignedWord) where

import Clash.Prelude hiding (Word)

type Nybble = Unsigned 4
type Byte = Unsigned 8
type TwoByte = Unsigned 16
type Word = Unsigned 64
type SignedWord = Signed 64
