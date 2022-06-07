module CPU.Core (cpuTopLvl, firstJust, maybeRamBlob, maybeRamFile, cpuRamRom) where

import qualified Prelude as P
import Clash.Prelude hiding (Word)

import Control.Monad.State (runState, State, get, gets, modify, put)
import Data.Maybe (fromMaybe)
import Clash.Signal.Internal (Signal(..))

import CPU.Types

type InpType = (Maybe RamType, Bool)
type OtpType = (RamAddrType, Maybe (RamAddrType, RamType))
type RamType = Byte
type RamAddrType = Word
type PC = RamAddrType
type RegData = Word
type Reg = Nybble
data CpuExecState = Start | ReadInstr | ExecInstr Byte | ExecReadRam RamAddrType Reg (Index 8) | ExecWriteRam RamAddrType (Vec (8-1) Byte) (Index (8-1)) deriving (Generic, NFDataX)
data CpuState = CpuState { regs :: Vec 16 RegData, execState :: CpuExecState, userMode :: Bool } deriving (Generic, NFDataX)
data Op =
    SoftInt
  | SwitchUser
  | ReadLit Reg
  | ReadRam Reg Reg
  | WriteRam Reg Reg
  | RegOp { regA :: Reg, regB :: Reg, regOp :: RegData -> RegData -> RegData }
  deriving (Generic, NFDataX)

pcReg :: CpuState -> Reg
pcReg cs = if (userMode cs) then 0xF else 0xE

pc :: CpuState -> Word
pc cs = (!! pcReg cs) (regs cs)

setPC :: RegData -> CpuState -> CpuState
setPC pc' cs = cs { regs = replace (pcReg cs) pc' (regs cs) }

addPC :: RegData -> CpuState -> CpuState
addPC n cs = setPC (pc cs + n) cs

incPC :: CpuState -> CpuState
incPC = addPC 2

wordToSigned :: Word -> SignedWord
wordToSigned = bitCoerce

opChunks :: TwoByte -> Vec 4 Nybble
opChunks = bitCoerce

signedBinop f x y = bitCoerce $ wordToSigned x `f` wordToSigned y

decodeBinopFn :: Nybble -> RegData -> RegData -> RegData
decodeBinopFn 0 = (\a b -> b)
-- decodeBinopFn 3 = (\x y -> x `shiftL` fromIntegral y) TODO find a way to do this without Ints
-- decodeBinopFn 4 = (\x y -> x `shiftR` fromIntegral y) TODO
decodeBinopFn 5 = (.&.)
decodeBinopFn 6 = (.|.)
decodeBinopFn 7 = xor
decodeBinopFn 8 = (+)
decodeBinopFn 9 = (-)
decodeBinopFn 0xA = (*)
decodeBinopFn 0xB = signedBinop (+)
decodeBinopFn 0xC = signedBinop (-)
decodeBinopFn 0xD = signedBinop (*)
{-# INLINE decodeBinopFn #-}

decodeCond :: Nybble -> RegData -> RegData -> Bool
decodeCond 0 x y = True
decodeCond 1 x y = x <= y
decodeCond 2 x y = wordToSigned x <= wordToSigned y
decodeCond _ _ _ = undefined
{-# INLINE decodeCond #-}

decodeOp :: TwoByte -> (Op, RegData -> RegData -> Bool)
decodeOp nyb = let chunks = opChunks nyb in case chunks of
  1   :> c :> a :> b :> Nil -> (ReadRam  a b, decodeCond c)
  2   :> c :> a :> b :> Nil -> (WriteRam a b, decodeCond c)
  0xF :> c :> 0 :> a :> Nil -> (ReadLit  a,   decodeCond c)
  0xF :> c :> 8 :> 0 :> Nil -> (SoftInt,      decodeCond c)
  0xF :> c :> 8 :> 1 :> Nil -> (SwitchUser,   decodeCond c)
  o   :> c :> a :> b :> Nil -> (RegOp { regA = a, regB = b, regOp = decodeBinopFn o }, decodeCond c)
{-# INLINE decodeOp #-}

nop :: Op
nop = fst $ decodeOp 0
{-# INLINE nop #-}

modifyOpUser :: Bool -> Op -> Op
modifyOpUser True (ReadRam a b) | a == 0xE || b == 0xE = nop
modifyOpUser True (WriteRam a b) | a == 0xE || b == 0xE = nop
modifyOpUser True (ReadLit 0xE) = nop
modifyOpUser True op@RegOp{} | regA op == 0xE || regB op == 0xE = nop
modifyOpUser _ op = op
{-# INLINE modifyOpUser #-}

cpuExecOp :: (Op, RegData -> RegData -> Bool) -> CpuState -> (CpuState, (RamAddrType, Maybe (RamAddrType, RamType)))
cpuExecOp (SoftInt, c) cs = (cs', (pc cs', Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1) && userMode cs
  csA = setPC 4 $ (incPC cs) { userMode = False, execState = ReadInstr }
  csB = incPC $ cs { execState = ReadInstr }
  cs' = if condCheck then csA else csB
cpuExecOp (SwitchUser, c) cs = (cs', (pc cs', Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1) && not (userMode cs)
  csA = cs { execState = ReadInstr, userMode = True }
  csB = incPC $ cs { execState = ReadInstr }
  cs' = if condCheck then csA else csB
cpuExecOp (ReadLit r, c) cs = (cs', (readAddr, Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1)
  cs' = addPC (8+2) $ cs { execState = if condCheck then ExecReadRam readAddr r 0 else ReadInstr }
  readAddr = pc cs + 2
cpuExecOp (ReadRam a b, c) cs = (cs', (if condCheck then readAddr else pc cs', Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1)
  cs' = incPC $ cs { execState = if condCheck then ExecReadRam readAddr a 0 else ReadInstr }
  readAddr = regs cs !! b
cpuExecOp (WriteRam a b, c) cs = (cs', (pc cs', if condCheck then Just (writeAddr, bytes !! 0) else Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1)
  cs' = incPC $ cs { execState = if condCheck then ExecWriteRam (writeAddr+1) (tail bytes) 0 else ReadInstr }
  writeAddr = regs cs !! a
  bytes = bitCoerce (regs cs !! b)
cpuExecOp (op@RegOp{}, c) cs = (cs', (pc cs', Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1)
  cs' = incPC $ cs { regs = if condCheck then newRegs else regs cs, execState = ReadInstr }
  newRegs = replace (regA op) (regOp op (regs cs !! regA op) (regs cs !! regB op)) (regs cs)
{-# INLINE cpuExecOp #-}

cpuMainFunc :: CpuState -> (RamType, Bool) -> (CpuState, (RamAddrType, Maybe (RamAddrType, RamType)))
cpuMainFunc cs@(CpuState { execState = Start }) _ = (cs { execState = ReadInstr }, (pc cs, Nothing))
cpuMainFunc cs@(CpuState { execState = ReadInstr }) (_, True) | userMode cs = (cs', (pc cs', Nothing)) where
  cs' = cs { userMode = False, regs = replace 0xE 2 (regs cs) }
cpuMainFunc cs@(CpuState { execState = ReadInstr }) (bitA, _) = (cs { execState = ExecInstr bitA }, (pc cs + 1, Nothing))
cpuMainFunc cs@(CpuState { execState = ExecInstr bitA }) (bitB, _) = cpuExecOp (op',c) cs where
  opNyb = bitCoerce (bitA, bitB)
  (op,c) = decodeOp opNyb
  op' = modifyOpUser (userMode cs) op
cpuMainFunc cs@(CpuState { execState = ExecReadRam _ r 7 {- = 8-1 -} }) (bit, _) = (cs', (pc cs', Nothing)) where
  cs' = cs { execState = ReadInstr, regs = replace r (((regs cs !! r) `shiftL` 8) .|. resize bit) (regs cs) }
cpuMainFunc cs@(CpuState { execState = ExecReadRam addr r amtLeft }) (bit, _) = (cs', (addr+1, Nothing)) where
  cs' = cs { execState = ExecReadRam (addr+1) r (amtLeft+1), regs = replace r (((regs cs !! r) `shiftL` 8) .|. resize bit) (regs cs) }
cpuMainFunc cs@(CpuState { execState = ExecWriteRam addr bytes n }) (bit, _) = (cs', (pc cs, Just (addr, bytes !! n))) where
  cs' = cs { execState = if n == maxBound then ReadInstr else ExecWriteRam (addr+1) bytes (n+1) }

cpuInitialState = CpuState { regs = repeat 0, execState = Start, userMode = False }

cpuTopLvl :: HiddenClockResetEnable dom => Signal dom RamType -> Signal dom Bool -> Signal dom (RamAddrType, Maybe (RamAddrType, RamType))
cpuTopLvl ia ib = mealy cpuMainFunc cpuInitialState $ bundle (ia, ib)

firstJust :: (KnownNat n) => Vec n (Signal dom (Maybe a)) -> Signal dom (Maybe a)
firstJust sig = f <$> bundle sig where
  f = foldl g Nothing
  g (Just a) b = Just a
  g Nothing b = b

maybeRamBlob :: (HiddenClockResetEnable dom) => SNat n -> MemBlob n 8 -> Signal dom RamAddrType -> Signal dom (Maybe (RamAddrType, RamType)) -> Signal dom (Maybe RamType)
maybeRamBlob ramSize blob readAddr write = f <$> register 0 readAddr <*> blockRamBlob blob readAddr (w <$> write) where
  f addr val = if addr >= (snatToNum ramSize) then Nothing else Just (unpack val)
  w (Just (addr, val)) | addr < (snatToNum ramSize) = Just (addr, pack val)
  w _ = Nothing

maybeRamFile :: (HiddenClockResetEnable dom) => SNat n -> FilePath -> Signal dom RamAddrType -> Signal dom (Maybe (RamAddrType, RamType)) -> Signal dom (Maybe RamType)
maybeRamFile ramSize path readAddr write = f <$> register 0 readAddr <*> blockRamFile ramSize path readAddr (w <$> write) where
  f addr val = if addr >= (snatToNum ramSize) then Nothing else Just (unpack val)
  w (Just (addr, val)) | addr < (snatToNum ramSize) = Just (addr, pack val)
  w _ = Nothing

cpuRamRom :: (HiddenClockResetEnable dom) => SNat n -> SNat m -> FilePath -> MemBlob m 8 -> Signal dom InpType -> Signal dom OtpType
cpuRamRom ramFileSize ramSize ramFilePath ramBlob inp = otp where
  (inpData, interrupt) = unbundle inp
  (ramReadAddr, ramWrite) = unbundle otp
  otp = cpuTopLvl ramRead interrupt
  ramRead = fromMaybe 0 <$> firstJust (inpData :> maybeRamFile ramFileSize ramFilePath ramReadAddr ramWrite :> maybeRamBlob ramSize ramBlob ramReadAddr ramWrite :> Nil)
