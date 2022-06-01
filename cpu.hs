import qualified Prelude as P
import Clash.Prelude hiding (Word)
import Control.Monad.State (runState, State, get, gets, modify, put)
import Data.Maybe (fromMaybe)

type HalfByte = Unsigned 4
type Byte = Unsigned 8
type Nybble = Unsigned 16
type Word = Unsigned 32
type InpType = (Maybe RamType, Bool)
type OtpType = (RamAddrType, Maybe (RamAddrType, RamType))
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
  | RegOp { regA :: Reg, regB :: Reg, regOp :: RegData -> RegData -> RegData }
  deriving (Generic, NFDataX)

pcReg :: CpuState -> Reg
pcReg cs = if (userMode cs) then 0xF else 0xE

pc :: CpuState -> Word
pc cs = (!! pcReg cs) (regs cs)

setPC :: RegData -> CpuState -> CpuState
setPC pc' cs = cs { regs = replace (pcReg cs) pc' (regs cs) }

incPC :: CpuState -> CpuState
incPC cs = setPC (pc cs + 2) cs

wordToSigned :: Word -> Signed 32
wordToSigned = unpack . pack

opChunks :: Nybble -> Vec 4 HalfByte
opChunks = unpack . pack

signedBinop f x y = unpack $ pack $ wordToSigned x `f` wordToSigned y

decodeBinopFn :: HalfByte -> RegData -> RegData -> RegData
decodeBinopFn 0 = const
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

decodeCond :: HalfByte -> RegData -> RegData -> Bool
decodeCond 0 x y = True
decodeCond 1 x y = x <= y
decodeCond 2 x y = wordToSigned x <= wordToSigned y
decodeCond _ _ _ = undefined
{-# INLINE decodeCond #-}

decodeOp :: Nybble -> (Op, RegData -> RegData -> Bool)
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
  cs' = incPC $ incPC $ incPC $ cs { execState = if condCheck then ExecReadRam readAddr r 0 else ReadInstr }
  readAddr = pc cs + 2
cpuExecOp (ReadRam a b, c) cs = (cs', (readAddr, Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1)
  cs' = incPC $ cs { execState = if condCheck then ExecReadRam readAddr a 0 else ReadInstr }
  readAddr = regs cs !! b
cpuExecOp (WriteRam a b, c) cs = (cs', (pc cs {- irrelevant -}, if condCheck then Just (writeAddr, bytes !! 0) else Nothing)) where
  condCheck = c (regs cs !! 0) (regs cs !! 1)
  cs' = incPC $ cs { execState = if condCheck then ExecWriteRam (writeAddr+1) (tail bytes) 0 else ReadInstr }
  writeAddr = regs cs !! a
  bytes = unpack (pack (regs cs !! b))
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
  opNyb = unpack (pack (bitA, bitB))
  (op,c) = decodeOp opNyb
  op' = modifyOpUser (userMode cs) op
cpuMainFunc cs@(CpuState { execState = ExecReadRam _ r 3 }) (bit, _) = (cs', (pc cs', Nothing)) where
  cs' = cs { execState = ReadInstr, regs = replace r (((regs cs !! r) `shiftL` 8) .|. resize bit) (regs cs) }
cpuMainFunc cs@(CpuState { execState = ExecReadRam addr r amtLeft }) (bit, _) = (cs', (addr+1, Nothing)) where
  cs' = cs { execState = ExecReadRam (addr+1) r (amtLeft+1), regs = replace r (((regs cs !! r) `shiftL` 8) .|. resize bit) (regs cs) }
cpuMainFunc cs@(CpuState { execState = ExecWriteRam addr bytes n }) (bit, _) = (cs', (pc cs, Just (addr, bytes !! n))) where
  cs' = cs { execState = if n == 2 then ReadInstr else ExecWriteRam (addr+1) bytes (n+1) }

cpuInitialState = CpuState { regs = repeat 0, execState = Start, userMode = False }

cpuTopLvl :: HiddenClockResetEnable dom => Signal dom RamType -> Signal dom Bool -> Signal dom (RamAddrType, Maybe (RamAddrType, RamType))
cpuTopLvl ia ib = mealy cpuMainFunc cpuInitialState $ bundle (ia, ib)

cpuRamRom :: (HiddenClockResetEnable dom, KnownNat n, KnownNat ramSize) => Vec n RamType -> SNat ramSize -> Signal dom InpType -> Signal dom OtpType
cpuRamRom rom ramSize inp = otp where
  (inpData, interrupt) = unbundle inp
  (ramReadAddr, ramWrite) = unbundle otp
  otp = cpuTopLvl inpData' interrupt
  ramReadData = blockRam (replicate ramSize 0) ramReadAddr ramWrite'
  ramWrite' = decideWrite <$> ramWrite
  decideWrite (Just (w,_)) | w >= (snatToNum ramSize) = Nothing
  decideWrite a = a
  inpData' = decideInput <$> ramReadData <*> register 0 ramReadAddr <*> inpData
  decideInput _ _ (Just a) = a
  decideInput _ n _ | n < snatToNum (lengthS rom) = rom !! n
  decideInput _ n _ | n >= snatToNum ramSize = 0
  decideInput ramData _ _ = ramData

code = 0xF0 :> 0x00 :>  0 :> 0 :> 0xFF :> 0xF0 :>  0x10 :> 0x10 :>  0x20 :> 0x01 :>  0xF0 :> 0x0E :>  0 :> 0 :> 0 :> 6 :>  Nil

topEntity' :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
topEntity' inp = otp where
  (readAddr, maybeWrite) = unbundle $ cpuRamRom code (SNat :: SNat 1000) cpuInput
  cpuInput = register (Nothing, False) $ genCpuInp <$> readAddr <*> inp
  genCpuInp 0xFFF3 i = (Just (fromIntegral i), False)
  genCpuInp _ _ = (Nothing, False)
  otp = regMaybe 0 (genOtpRegVal <$> maybeWrite)
  genOtpRegVal (Just (0xFFF3, v)) = Just (if v == 0 then 0 else 1)
  genOtpRegVal _ = Nothing

createDomain vSystem{vName="Dom100", vPeriod=10000}

topEntity :: ("clk" ::: Clock Dom100) -> ("btn" ::: Signal Dom100 Bit) -> ("led" ::: Signal Dom100 Bit)
topEntity clk btn = withClockResetEnable clk resetGen enableGen topEntity' btn
