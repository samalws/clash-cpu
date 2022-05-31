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
    DebugOtp Reg
  | SoftInt
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
-- decodeBinopFn 1 = (\x y -> x `shiftL` fromIntegral y) TODO find a way to do this without Ints
-- decodeBinopFn 2 = (\x y -> x `shiftR` fromIntegral y) TODO
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
decodeBinopCond _ _ _ = undefined
{-# INLINE decodeBinopCond #-}

decodeOp :: Nybble -> Op
decodeOp nyb = let chunks = opChunks nyb in case chunks of
  0xA :> 0xA :> 0xA :> a :> Nil -> DebugOtp a
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
cpuExecOp (DebugOtp r) cs = (cs', (pc cs', Just (regs cs !! r, 0))) where
  cs' = incPC $ cs { execState = ReadInstr }
cpuExecOp SoftInt cs | userMode cs = (cs', (pc cs', Nothing)) where
  cs' = setPC 4 $ (incPC cs) { userMode = False, execState = ReadInstr }
cpuExecOp SwitchUser cs | not (userMode cs) = (cs', (pc cs', Nothing)) where
  cs' = cs { execState = ReadInstr, userMode = True }
cpuExecOp (ReadLit r) cs = (cs', (readAddr, Nothing)) where
  cs' = incPC $ incPC $ incPC $ cs { execState = ExecReadRam readAddr r 0 }
  readAddr = pc cs + 2
cpuExecOp (ReadRam a b) cs = (cs', (readAddr, Nothing)) where
  cs' = incPC $ cs { execState = ExecReadRam readAddr a 0 }
  readAddr = regs cs !! b
cpuExecOp (WriteRam a b) cs = (cs', (pc cs {- irrelevant -}, Just (writeAddr, bytes !! 0))) where
  cs' = incPC $ cs { execState = ExecWriteRam (writeAddr+1) (tail bytes) 0 }
  writeAddr = regs cs !! a
  bytes = unpack (pack (regs cs !! b))
cpuExecOp op@(RegOp{}) cs = (cs', (pc cs', Nothing)) where
  cs' = incPC $ cs { regs = if condCheck then newRegs else regs cs, execState = ReadInstr }
  condCheck = opCond op (regs cs !! 0) (regs cs !! 1)
  newRegs = replace (regA op) (regOp op (regs cs !! regA op) (regs cs !! regB op)) (regs cs)
{-# INLINE cpuExecOp #-}

cpuMainFunc :: CpuState -> (RamType, Bool) -> (CpuState, (RamAddrType, Maybe (RamAddrType, RamType)))
cpuMainFunc cs@(CpuState { execState = Start }) _ = (cs { execState = ReadInstr }, (pc cs, Nothing))
cpuMainFunc cs@(CpuState { execState = ReadInstr }) (_, True) | userMode cs = (cs', (pc cs', Nothing)) where
  cs' = cs { userMode = False, regs = replace 0xE 2 (regs cs) }
cpuMainFunc cs@(CpuState { execState = ReadInstr }) (bitA, _) = (cs { execState = ExecInstr bitA }, (pc cs + 1, Nothing))
cpuMainFunc cs@(CpuState { execState = ExecInstr bitA }) (bitB, _) = cpuExecOp op cs where
  opNyb = unpack (pack (bitA, bitB))
  op = modifyOpUser (userMode cs) (decodeOp opNyb)
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
  inpData' = register undefined $ decideInput <$> ramReadData <*> ramReadAddr <*> inpData
  decideInput _ _ (Just a) = a
  decideInput _ n _ | n < snatToNum (lengthS rom) = rom !! n
  decideInput _ n _ | n >= snatToNum ramSize = 0
  decideInput ramData _ _ = ramData

code = 0xE0 :> 0x00 :>  0 :> 0 :> 0xFF :> 0xF0 :>  0xE1 :> 0x10 :>  0xE2 :> 0x01 :>  0xE0 :> 0x0E :>  0 :> 0 :> 0 :> 6 :>  Nil

topEntity' :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
topEntity' inp = otp where
  (readAddr, maybeWrite) = unbundle $ cpuRamRom code (SNat :: SNat 1000) cpuInput
  cpuInput = register (Nothing, False) $ genCpuInp <$> readAddr <*> inp
  genCpuInp 0xFFF2 i = (Just (fromIntegral i), False) -- TODO uhhh why not 0xFFF3?
  genCpuInp _ _ = (Nothing, False)
  otp = regMaybe 0 (genOtpRegVal <$> maybeWrite)
  genOtpRegVal (Just (0xFFF3, v)) = Just (if v == 0 then 0 else 1)
  genOtpRegVal _ = Nothing

createDomain vSystem{vName="Dom100", vPeriod=10000}

topEntity :: ("clk" ::: Clock Dom100) -> ("btn" ::: Signal Dom100 Bit) -> ("led" ::: Signal Dom100 Bit)
topEntity clk btn = withClockResetEnable clk resetGen enableGen topEntity' btn
