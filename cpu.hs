{-# LANGUAGE LambdaCase #-}

import qualified Prelude as P
import Clash.Prelude hiding (Word)
import Control.Monad.State (runState, State, get, gets, modify, put)
import Data.Maybe (fromMaybe)

createDomain vSystem{vName="Dom100", vPeriod=10000}

type HalfByte = Unsigned 4
type Byte = Unsigned 8
type Nybble = Unsigned 16
type Word = Unsigned 32
type InpType = (Maybe RamType, Bool)
type OtpType = Maybe (RamAddrType, RamType)
type RamType = Byte
type RamAddrType = Word
type PC = RamAddrType
type RegData = Word
type Reg = HalfByte
data CpuExecState = Start | ReadInstr | ExecInstr Byte | ExecReadRam RamAddrType Reg (Index 4) | ExecWriteRam RamAddrType (Vec 3 Byte) (Index 3) deriving (Generic, NFDataX)
data CpuState = CpuState { regs :: Vec 16 RegData, execState :: CpuExecState, userMode :: Bool } deriving (Generic, NFDataX)
data Op =
    SoftInt
  | SwitchUser
  | ReadLit Reg
  | ReadRam Reg Reg
  | WriteRam Reg Reg
  | RegOp { regA :: Reg, regB :: Reg, regOp :: RegData -> RegData -> RegData, opCond :: RegData -> RegData -> Bool }
  deriving (Generic, NFDataX)

pcReg :: CpuState -> Reg
pcReg cs = if (userMode cs) then 0xF else 0xE

pc :: CpuState -> Word
pc cs = (!! pcReg cs) (regs cs)

wordToSigned :: Word -> Signed 32
wordToSigned = unpack . pack

wordToFloat :: Word -> Float
wordToFloat = unpack . pack

opChunks :: Nybble -> Vec 4 HalfByte
opChunks = unpack . pack

signedBinop f x y = unpack $ pack $ wordToSigned x `f` wordToSigned y

decodeBinopFn :: HalfByte -> RegData -> RegData -> RegData
decodeBinopFn 0 = const
decodeBinopFn 1 = (\x y -> x `shiftL` fromIntegral y)
decodeBinopFn 2 = (\x y -> x `shiftR` fromIntegral y)
decodeBinopFn 3 = (.&.)
decodeBinopFn 4 = (.|.)
decodeBinopFn 5 = xor
decodeBinopFn 6 = (+)
decodeBinopFn 7 = (-)
decodeBinopFn 8 = (*)
decodeBinopFn 9 = signedBinop (+)
decodeBinopFn 10 = signedBinop (-)
decodeBinopFn 11 = signedBinop (*)
{-# INLINE decodeBinopFn #-}

decodeBinopCond :: HalfByte -> RegData -> RegData -> Bool
decodeBinopCond 0 x y = True
decodeBinopCond 1 x y = x <= y
decodeBinopCond 2 x y = wordToSigned x <= wordToSigned y
-- decodeBinopCond 3 x y = wordToFloat x <= wordToFloat y
decodeBinopCond _ _ _ = undefined
{-# INLINE decodeBinopCond #-}

decodeOp :: Nybble -> Op
decodeOp nyb = let chunks = opChunks nyb in case chunks of
  0xE :> 0 :> 0 :> a :> Nil -> ReadLit a
  0xE :> 1 :> a :> b :> Nil -> ReadRam a b
  0xE :> 2 :> a :> b :> Nil -> WriteRam a b
  0xF :> 0 :> 0 :> 0 :> Nil -> SoftInt
  0xF :> 0 :> 0 :> 1 :> Nil -> SwitchUser
  c   :> o :> a :> b :> Nil -> RegOp { regA = a, regB = b, regOp = decodeBinopFn o, opCond = decodeBinopCond c }
{-# INLINE decodeOp #-}

nop :: Op
nop = decodeOp 0
{-# INLINE nop #-}

modifyOpUser :: Bool -> Op -> Op
modifyOpUser True op@RegOp{} | regA op == 0xE || regB op == 0xE = nop
modifyOpUser _ op = op
{-# INLINE modifyOpUser #-}

cpuExecOp :: Op -> CpuState -> (CpuState, (RamAddrType, Maybe (RamAddrType, RamType)))
cpuExecOp SoftInt cs | userMode cs = (newCS, (pc newCS, Nothing)) where
  newCS = cs { regs = replace 0xF (pc cs + 2) $ replace 0xE 4 $ regs cs, execState = ReadInstr, userMode = False }
cpuExecOp SwitchUser cs | not (userMode cs) = (newCS, (pc newCS, Nothing)) where
  newCS = cs { execState = ReadInstr, userMode = True }
cpuExecOp (ReadLit r) cs = (newCS, (readAddr, Nothing)) where
  newCS = cs { execState = ExecReadRam readAddr r 0, regs = regs' }
  regs' = replace (pcReg cs) (pc cs + 6) (regs cs)
  readAddr = pc cs + 2
cpuExecOp (ReadRam a b) cs = (newCS, (readAddr, Nothing)) where
  newCS = cs { execState = ExecReadRam readAddr a 0, regs = regs' }
  regs' = replace (pcReg cs) (pc cs + 2) (regs cs)
  readAddr = regs cs !! b
cpuExecOp (WriteRam a b) cs = (newCS, (pc cs {- irrelevant -}, Just (writeAddr, bytes !! 0))) where
  newCS = cs { execState = ExecWriteRam (writeAddr+1) (tail bytes) 0, regs = regs' }
  regs' = replace (pcReg cs) (pc cs + 2) (regs cs)
  writeAddr = regs cs !! a
  bytes = unpack (pack (regs cs !! b))
cpuExecOp op@(RegOp{}) cs = (newCS, (newPC, Nothing)) where
  newCS = cs { regs = newRegs'', execState = ReadInstr }
  condCheck = opCond op (regs cs !! 0) (regs cs !! 1)
  newRegs = replace (regA op) (regOp op (regs cs !! regA op) (regs cs !! regB op)) (regs cs)
  newRegs' = if condCheck then newRegs else regs cs
  newPC = newRegs' !! (pcReg cs) + 2
  newRegs'' = replace (pcReg cs) newPC newRegs'
{-# INLINE cpuExecOp #-}

cpuMainFunc :: CpuState -> (RamType, Bool) -> (CpuState, (RamAddrType, Maybe (RamAddrType, RamType)))
cpuMainFunc cs@(CpuState { execState = Start }) _ = (cs { execState = ReadInstr }, (pc cs, Nothing))
cpuMainFunc cs@(CpuState { execState = ReadInstr }) (_, True) | userMode cs = (cs', (pc cs', Nothing)) where
  cs' = cs { userMode = False, regs = replace 0xE 2 (regs cs) }
cpuMainFunc cs@(CpuState { execState = ReadInstr }) (bitA, _) = (cs { execState = ExecInstr bitA }, (pc cs + 1, Nothing))
cpuMainFunc cs@(CpuState { execState = ExecInstr bitA }) (bitB, _) = cpuExecOp op cs where
  opNyb = unpack (pack (bitA, bitB))
  op = modifyOpUser (userMode cs) (decodeOp opNyb)
cpuMainFunc cs@(CpuState { execState = ExecReadRam _ r 3 }) (bit, _) = (cs', (pc cs, Nothing)) where
  cs' = cs { execState = ReadInstr, regs = replace r (((regs cs !! r) `shiftL` 8) .|. resize bit) (regs cs) }
cpuMainFunc cs@(CpuState { execState = ExecReadRam addr r amtLeft }) (bit, _) = (cs', (addr+1, Nothing)) where
  cs' = cs { execState = ExecReadRam (addr+1) r (amtLeft+1), regs = replace r (((regs cs !! r) `shiftL` 8) .|. resize bit) (regs cs) }
cpuMainFunc cs@(CpuState { execState = ExecWriteRam addr bytes n }) (bit, _) = (cs', (pc cs, Just (addr, bytes !! n))) where
  cs' = cs { execState = if n == 2 then ReadInstr else ExecWriteRam (addr+1) bytes (n+1) }

cpuInitialState = CpuState { regs = repeat 0, execState = Start, userMode = True }

cpuLogic :: HiddenClockResetEnable dom => Signal dom RamType -> Signal dom Bool -> Signal dom (RamAddrType, Maybe (RamAddrType, RamType))
cpuLogic ia ib = mealy cpuMainFunc cpuInitialState $ bundle (ia, ib)

initialRamState :: Vec 1000 RamType
initialRamState = repeat 0

topEntity' :: HiddenClockResetEnable dom => Signal dom InpType -> Signal dom OtpType
topEntity' inp = otp where
  otp = ramWrite
  (ramReadAddr, ramWrite) = unbundle $ cpuLogic ramReadData (snd <$> inp)
  ramReadData = blockRam initialRamState ramReadAddr ramWrite
  ramReadData' = fromMaybe <$> ramReadData <*> (fst <$> inp)

topEntity :: Signal Dom100 InpType -> Signal Dom100 OtpType
topEntity = withClockResetEnable clockGen resetGen enableGen topEntity'
