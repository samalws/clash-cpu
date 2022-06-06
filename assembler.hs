import Prelude hiding (read, Word)
import Clash.Prelude hiding (length, replicate, read, Word)
import Control.Monad.State
import Control.Monad.Writer

type Nybble = Unsigned 4
type Byte = Unsigned 8
type Word = Unsigned 64
type Assembler a = WriterT [Byte] (State Word) a
data Cond = A | LE | SLE

label :: Assembler Word
label = get

condToNyb :: Cond -> Nybble
condToNyb A = 0
condToNyb LE = 1
condToNyb SLE = 2

addBytes :: [Byte] -> Assembler ()
addBytes bs = tell bs >> modify (+ fromIntegral (length bs))

addInstr :: Vec 4 Nybble -> Assembler ()
addInstr (a :> b :> c :> d :> Nil) = addBytes $ bitCoerce <$> [(a,b), (c,d)]

standardInstr :: Nybble -> Cond -> Nybble -> Nybble -> Assembler ()
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
lit :: Cond -> Nybble -> Word -> Assembler ()
lit c a v = standardInstr 0xF c 0 a >> addBytes [b1,b2,b3,b4,b5,b6,b7,b8] where (b1,b2,b3,b4,b5,b6,b7,b8) = bitCoerce v
int c = standardInstr 0xF c 8 0
user c = standardInstr 0xF c 8 1

jmp  c lbl = lit c 0xF lbl -- at user permission
jmpK c lbl = lit c 0xE lbl -- at kernel permission

-- swap a and b under condition c, clobbering z
swap c a b z = do
  mov c z a
  mov c a b
  mov c b z

runAssembler :: Assembler () -> [Byte]
runAssembler = snd . fst . flip runState 0 . runWriterT

padAssembly :: Int -> [Byte] -> [Byte]
padAssembly n bs = bs <> replicate (n - length bs) 0

assemblyToFile :: [Byte] -> String
assemblyToFile = unlines . fmap (binaryShow . bitCoerce) where
  binaryShow :: (Bit,Bit,Bit,Bit,Bit,Bit,Bit,Bit) -> String
  binaryShow (a,b,c,d,e,f,g,h) = show a <> show b <> show c <> show d <> show e <> show f <> show g <> show h

toAssemble = do
  lit A 6 0
  lit A 7 1
  lit A 8 0xFFF0
  lit A 9 0xFFF4
  beginning <- label
  write A 8 6
  write A 9 6
  read A 0 8
  read A 1 9
  write LE 8 7
  swap A 0 1 2
  write LE 9 7
  jmpK A beginning

main = writeFile "meminit.bin" $ assemblyToFile $ padAssembly 100 $ runAssembler toAssemble
