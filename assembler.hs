import Prelude hiding (read, Word)
import Clash.Prelude hiding (length, replicate, read, Word)
import Control.Monad.State
import Control.Monad.Writer

type HalfByte = Unsigned 4
type Byte = Unsigned 8
type Word = Unsigned 32
type Assembler a = WriterT [Byte] (State Word) a
data Cond = A | LE | SLE

label :: Assembler Word
label = get

condToNyb :: Cond -> HalfByte
condToNyb A = 0
condToNyb LE = 1
condToNyb SLE = 2

addBytes :: [Byte] -> Assembler ()
addBytes bs = tell bs >> modify (+ fromIntegral (length bs))

addInstr :: Vec 4 HalfByte -> Assembler ()
addInstr (a :> b :> c :> d :> Nil) = addBytes $ unpack . pack <$> [(a,b), (c,d)]

standardInstr :: HalfByte -> Cond -> HalfByte -> HalfByte -> Assembler ()
standardInstr a b c d = addInstr (a :> condToNyb b :> c :> d :> Nil)

mov = standardInstr 0
read = standardInstr 1
write = standardInstr 2
shl = standardInstr 3
shr = standardInstr 4
band = standardInstr 5
bor = standardInstr 6
bxor = standardInstr 7
add = standardInstr 8
sub = standardInstr 9
mul = standardInstr 0xA
sadd = standardInstr 0xB
ssub = standardInstr 0xC
smul = standardInstr 0xD
lit :: Cond -> HalfByte -> Word -> Assembler ()
lit c a v = standardInstr 0xF c 0 a >> addBytes [w,x,y,z] where (w,x,y,z) = unpack $ pack v
int c = standardInstr 0xF c 8 0
user c = standardInstr 0xF c 8 1

jmp  c lbl = lit c 0xF lbl
jmpK c lbl = lit c 0xE lbl

runAssembler :: Assembler () -> [Byte]
runAssembler = (\((_,a),_) -> a) . flip runState 0 . runWriterT

padAssembly :: Int -> [Byte] -> [Byte]
padAssembly n bs = bs <> replicate (n - length bs) 0

assemblyToFile :: [Byte] -> String
assemblyToFile = unlines . fmap (binaryShow . unpack . pack) where
  binaryShow :: (Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit) -> String
  binaryShow (a,b,c,d,e,f,g,h) = show a <> show b <> show c <> show d <> show e <> show f <> show g <> show h

toAssemble = do
  lit A 0 0xFFF0
  beginning <- label
  read A 1 0
  write A 0 1
  jmpK A beginning

main = writeFile "meminit.bin" $ assemblyToFile $ padAssembly 50 $ runAssembler toAssemble
