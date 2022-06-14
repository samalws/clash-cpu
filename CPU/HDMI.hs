module CPU.HDMI (loopingBuffer, HDMIOtp, hdmi) where

-- TODO multiple_hidden

import qualified Prelude as P
import Clash.Prelude hiding (trueDualPortBlockRam)
import Clash.Explicit.Prelude (trueDualPortBlockRam)

import Data.Maybe (isNothing)

import CPU.Types (Byte)

type HDMIWord = Unsigned 10
data HDMIOtp = HDMIOtp { clkP :: Bit, clkN :: Bit, d0P :: Bit, d0N :: Bit, d1P :: Bit, d1N :: Bit, d2P :: Bit, d2N :: Bit } deriving (Generic, BitPack, NFDataX)

reverseHDMIWord :: HDMIWord -> HDMIWord
reverseHDMIWord = bitCoerce . bitCoerceMap (reverse :: Vec 10 Bit -> Vec 10 Bit)

genHDMIOtp :: (Bit, Vec 3 Bit) -> HDMIOtp
genHDMIOtp (a, b :> c :> d :> Nil) = HDMIOtp a (1-a) b (1-b) c (1-c) d (1-d)

type HdmiScreenSize (width :: Nat) (height :: Nat) (sideWidth :: Nat) (sideHeight :: Nat) = (width + sideWidth) * (height + sideHeight)

inc :: (KnownNat n) => Index n -> Index n
inc n = if n == maxBound then 0 else n+1

counter :: (HiddenClockResetEnable dom, KnownNat n) => Signal dom (Index n)
counter = register 0 $ inc <$> counter

infixl 3 .<|>.
a .<|>. b = (<|>) <$> a <*> b


loopingBuffer :: (KnownDomain domA, KnownDomain domB, NFDataX a, KnownNat n) =>
  Clock domA -> Clock domB -> Enable domB -> Signal domA (Maybe (Index n, a)) -> Signal domB a
loopingBuffer clkA clkB enB writes
  = snd $ trueDualPortBlockRam
          clkA clkB
          (maybe RamNoOp (uncurry RamWrite) <$> writes)
          (RamRead <$> withClockResetEnable clkB resetGen enB counter)

hdmiControlPeriod :: (Bit,Bit) -> HDMIWord
hdmiControlPeriod (0,0) = 0b1101010100
hdmiControlPeriod (1,0) = 0b0010101011
hdmiControlPeriod (0,1) = 0b0101010100
hdmiControlPeriod (1,1) = 0b1010101011

hdmiControlPeriodHsync = hdmiControlPeriod <$> ((1,0) :> (0,0) :> (0,0) :> Nil)
hdmiControlPeriodVsync = hdmiControlPeriod <$> ((0,1) :> (0,0) :> (0,0) :> Nil)
hdmiControlPeriodPreamble = hdmiControlPeriod <$> ((0,0) :> (1,0) :> (0,0) :> Nil)
hdmiVideoGuardBand = 0b1011001100 :> 0b0100110011 :> 0b1011001100 :> Nil :: Vec 3 HDMIWord

-- for each hsync period: a bunch of hdmiControlPeriodHsync :- 8 x hdmiControlPeriodPreamble :- 2 x hdmiVideoGuardBand
-- for the big vsync period: a bunch of hdmiControlPeriodVsync :- 8 x hdmiControlPeriodPreamble :- 2 x hdmiVideoGuardBand

hdmiXorPixel :: (KnownNat n, n ~ (decN + 1)) => Vec n Bit -> Vec (n+1) Bit
hdmiXorPixel vec = foldl (\v x -> v <<+ x `xor` last v) (repeat 0) vec ++ (0 :> Nil)

hdmiXnorPixel :: (KnownNat n, n ~ (decN + 1)) => Vec n Bit -> Vec (n+1) Bit
hdmiXnorPixel vec = foldl (\v x -> v <<+ 1 - (x `xor` last v)) (repeat 0) vec ++ (1 :> Nil)

numTransitions :: (KnownNat n) => Vec n Bit -> Index n
numTransitions = fst . foldl (\(n,x) y -> if x == (Just y) then (n+1,Just y) else (n,Just y)) (0, Nothing)

hdmiTransitionOptimizeByte :: Vec 8 Bit -> Vec 9 Bit
hdmiTransitionOptimizeByte byte = if (numTransitions a < numTransitions b) then a else b where
  a = hdmiXorPixel byte
  b = hdmiXnorPixel byte

hdmiPixel :: Byte -> HDMIWord
hdmiPixel b = bitCoerce $ (hdmiXorPixel $ bitCoerce b) ++ (0 :> Nil) -- TODO optionally flip

convScreenCoords :: (KnownNat width, KnownNat height, KnownNat sideWidth, KnownNat sideHeight, Num n) =>
  (SNat width, SNat height, SNat sideWidth, SNat sideHeight) -> n -> n -> n
convScreenCoords (width, height, sideWidth, _) x y = x + (y * snatToNum (width `addSNat` sideWidth))

data HdmiInitState (width :: Nat) (height :: Nat) (sideWidth :: Nat) (sideHeight :: Nat)
  = InitStart
  | InitHsync (Index sideWidth) (Index height)
  | InitVsync (Index (width + sideWidth)) (Index sideHeight)
  | InitPreamble (Index 8) (Index height)
  | InitGuardBand (Index 2) (Index height)
  | InitDone -- just for typecheck reasons
  deriving (Generic, NFDataX)

-- TODO type level hack
initialHdmiInitState :: (KnownNat width, KnownNat height, KnownNat sideWidth, KnownNat sideHeight) =>
  (SNat width, SNat height, SNat sideWidth, SNat sideHeight) ->
  HdmiInitState width height sideWidth sideHeight
initialHdmiInitState _ = InitStart

hdmiInit :: (HiddenClockResetEnable domA, KnownNat width, KnownNat height, KnownNat sideWidth, KnownNat sideHeight, sideWidth ~ (sideWidthSub10 + 10), sideHeight ~ (decSideHeight + 1)) =>
  (SNat width, SNat height, SNat sideWidth, SNat sideHeight) ->
  Signal domA (Maybe (Index (HdmiScreenSize width height sideWidth sideHeight), Vec 3 HDMIWord))
hdmiInit size@(width, height, sideWidth, sideHeight) = moore trans otp (initialHdmiInitState size) (pure ()) where

  otp InitStart = Just (convScreenCoords size (snatToNum width) 0, hdmiControlPeriodHsync)
  otp (InitHsync x y) = Just (convScreenCoords size (snatToNum width + fromIntegral x) (fromIntegral y), hdmiControlPeriodHsync)
  otp (InitVsync x y) = Just (convScreenCoords size (fromIntegral x) (snatToNum height + fromIntegral y), hdmiControlPeriodVsync)
  otp (InitPreamble x y) | y == maxBound = Just (convScreenCoords size (snatToNum (width `addSNat` sideWidth `subSNat` (SNat @10)) + fromIntegral x) (snatToNum (height `addSNat` sideHeight `subSNat` SNat @1)), hdmiControlPeriodPreamble)
  otp (InitPreamble x y) = Just (convScreenCoords size (snatToNum (width `addSNat` sideWidth `subSNat` (SNat @10)) + fromIntegral x) (fromIntegral y), hdmiControlPeriodPreamble)
  otp (InitGuardBand x y) | y == maxBound = Just (convScreenCoords size (snatToNum (width `addSNat` sideWidth `subSNat` (SNat @2)) + fromIntegral x) (snatToNum (height `addSNat` sideHeight `subSNat` SNat @1)), hdmiVideoGuardBand)
  otp (InitGuardBand x y) = Just (convScreenCoords size (snatToNum (width `addSNat` sideWidth `subSNat` (SNat @2)) + fromIntegral x) (fromIntegral y), hdmiVideoGuardBand)

  trans InitStart () = InitHsync 0 0
  trans (InitHsync x y) () = trans' InitHsync (InitVsync 0 0) x y
  trans (InitVsync x y) () = trans' InitVsync (InitPreamble 0 0) x y
  trans (InitPreamble x y) () = trans' InitPreamble (InitGuardBand 0 0) x y
  trans (InitGuardBand x y) () = trans' InitGuardBand InitDone x y
  trans InitDone () = InitDone

  trans' f g x y
    | x == maxBound && y == maxBound = g
    | x == maxBound = f 0 (y+1)
    | otherwise = f (x+1) y

hdmiPixelBuffer :: (KnownDomain domA, KnownDomain domB, KnownNat width, KnownNat height, KnownNat sideWidth, KnownNat sideHeight, sideWidth ~ (sideWidthSub10 + 10), sideHeight ~ (decSideHeight + 1)) =>
  (SNat width, SNat height, SNat sideWidth, SNat sideHeight) ->
  Clock domA -> Clock domB -> Enable domB ->
  Signal domA (Maybe (Index width, Index height, Vec 3 Byte)) ->
  (Signal domA Bool, Signal domB (Vec 3 HDMIWord))
hdmiPixelBuffer screenSize clkA clkB enB write
  = (isNothing <$> init',
     loopingBuffer clkA clkB enB (reverseWordInp $ init' .<|>. (fmap processWrite <$> write))) where

  init' = withClockResetEnable clkA resetGen enableGen (hdmiInit screenSize)

  processWrite (x,y,c) =
    (convScreenCoords screenSize (fromIntegral x) (fromIntegral y),
     hdmiPixel <$> c)

  reverseWordInp = fmap $ fmap $ fmap $ fmap reverseHDMIWord

bitIdx :: (BitPack a, KnownNat (BitSize a)) => Index (BitSize a) -> a -> Bit
bitIdx i a = bitCoerce a !! i

hdmi :: (KnownDomain domA, KnownDomain domB, KnownNat width, KnownNat height, KnownNat sideWidth, KnownNat sideHeight, sideWidth ~ (sideWidthSub10 + 10), sideHeight ~ (decSideHeight + 1)) =>
  (SNat width, SNat height, SNat sideWidth, SNat sideHeight) ->
  Clock domA -> Clock domB ->
  Signal domA (Maybe (Index width, Index height, Vec 3 Byte)) ->
  (Signal domA Bool, Signal domB HDMIOtp)
hdmi screenSize clkA clkB write = (init, delayOtp $ processOtp <$> bitCounter <*> pixs) where

  delayOtp = withClockResetEnable clkB resetGen enableGen $ register (genHDMIOtp (0,repeat 0)) 

  (init,pixs) = hdmiPixelBuffer screenSize clkA clkB enable write

  bitCounter = withClockResetEnable clkB resetGen enableGen counter
  enable = toEnable $ (maxBound ==) <$> bitCounter

  processOtp bitNum vec = genHDMIOtp (bitCoerce (bitNum >= 5), bitIdx bitNum <$> vec)
