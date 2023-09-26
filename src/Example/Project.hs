{-# LANGUAGE DerivingStrategies #-}
module Example.Project (topEntity, plus) where

import Clash.Prelude hiding (Word, cycle)
import Clash.Explicit.Testbench
import Clash.Num.Saturating (Saturating)
import qualified Clash.Promoted.Nat.Literals as Nat

import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Vector (Vec((:>), Nil),
        (!!), replace, repeat, (++))
import Clash.Class.Resize (zeroExtend)
import Clash.Sized.BitVector (BitVector, (++#), Bit)
import Clash.Class.BitPack (pack, unpack)
import Clash.Prelude (slice)
import Clash.Signal (Signal, register, sample)

-- Plain old Haskell stuff
import Prelude ((+), (-), (*), (==), ($), (.),
    filter, take, fmap, not, error,
    Show,  Bool(True,False), Maybe(Just,Nothing))


type Matrix m n a = Vec m (Vec n a)

mMul :: KnownNat m
  => KnownNat n
  => KnownNat p
  => Num a
  => Matrix m n a
  -> Matrix n p a
  -> Matrix m p a
mMul = undefined


-- | Add two numbers. Example:
--
-- >>> plus 3 5
-- 8
plus :: Signed 8 -> Signed 8 -> Signed 8
plus a b = a + b

ma :: Num a => a -> (a, a) -> a
ma acc (x, y) = acc + x*y

macT :: Num a => a -> (a, a) -> (a, a)
macT acc (x, y) = (acc', o)
  where
    acc' = ma acc (x, y)
    o = acc


mac inp = mealy macT 0 inp


dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedAdd (zipWith boundedMul as bs)


fir :: (HiddenClockResetEnable dom, KnownNat n, SaturatingNum a, NFDataX a, Default a)
        => Vec (n + 1) a -> Signal dom a -> Signal dom a
fir coeefs x_t = y_t
  where
    y_t = dotp coeefs <$> bundle xs
    xs = window x_t


-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.


coeff_t = 10 :> 11 :> 11 :> 8 :> 3 :> -3 :> -8 :> -11 :> -11 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -11 :> -11 :> -8 :> -3 :> 3 :> 8 :> 11 :> 11 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 11 :> 11 :> 8 :> 3 :> -3 :> -8 :> -11 :> -11 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -11 :> -11 :> -8 :> -3 :> 3 :> 8 :> 11 :> 11 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 11 :> 11 :> 8 :> 3 :> -3 :> -8 :> -11 :> -11 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -10 :> -11 :> -11 :> -8 :> -3 :> 3 :> 8 :> 11 :> 11 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> 10 :> Nil

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 22)
  -> Signal System (Signed 22)
topEntity = exposeClockResetEnable (fir coeff_t)


data Register = R1 | R2 | R3 | R4 
  deriving stock (Generic, Show)
  deriving anyclass NFDataX

newtype Ptr = Ptr (Unsigned 64) 
  deriving stock (Generic, Show)
  deriving anyclass NFDataX

newtype Word = Word (Unsigned 64)
  deriving stock (Generic, Show)
  deriving anyclass NFDataX

newtype Output = Output (Unsigned 64)
  deriving stock (Generic, Show)
  deriving anyclass NFDataX

data Instruction
    = LoadIm Register (Unsigned 56)
    | Add Register Register Register
    | Sub Register Register Register
    | Mul Register Register Register
    | Load Register Register
    | Store Register Register
    | Jmp Register
    | JmpZ Register Register
    | Out Register
    | Halt
    deriving stock (Generic, Show)
    deriving anyclass NFDataX


data CPUActivity
  = LoadingInstruction
  | ExecutingInstruction Instruction
  | ReadingMemory Ptr Register
  | WritingMemory Ptr Word
  | Outputting Output
  | Halted
  deriving stock (Generic, Show)
  deriving anyclass NFDataX


data Registers = Registers
  { r1 :: Unsigned 64
  , r2 :: Unsigned 64
  , r3 :: Unsigned 64
  , r4 :: Unsigned 64
  , pc :: Ptr
} deriving stock (Generic, Show)
  deriving anyclass NFDataX

data CPUState = CPUState CPUActivity Registers
  deriving stock (Generic, Show)
  deriving anyclass NFDataX

data RAM = RAM (Vec 64 Word)
  deriving stock (Generic, Show)
  deriving anyclass NFDataX

readRegister :: Registers -> Register -> Unsigned 64
readRegister (Registers reg1 reg2 reg3 reg4 _) reg = case reg of
    R1 -> reg1
    R2 -> reg2
    R3 -> reg3
    R4 -> reg4

writeRegister :: Registers -> Register -> Unsigned 64 -> Registers
writeRegister regs reg word = case reg of
    R1 -> regs {r1 = word}
    R2 -> regs {r2 = word}
    R3 -> regs {r3 = word}
    R4 -> regs {r4 = word}

readRAM :: RAM -> Ptr -> Word
readRAM (RAM contents) (Ptr address) = contents !! address

writeRAM :: RAM -> Ptr -> Word -> RAM
writeRAM (RAM contents) (Ptr address) val = RAM (replace address val contents)

increment :: Ptr -> Ptr
increment (Ptr address) = Ptr (address + 1)

encodeInstruction :: Instruction -> Word
encodeInstruction instr = Word $ unpack $ case instr of
    LoadIm r v -> tag 0 ++# encodeReg r ++# pack v
    Add  a b d -> tag 1 ++# encodeReg a ++# encodeReg b ++# encodeReg d ++# 0
    Sub  a b d -> tag 2 ++# encodeReg a ++# encodeReg b ++# encodeReg d ++# 0
    Mul  a b d -> tag 3 ++# encodeReg a ++# encodeReg b ++# encodeReg d ++# 0
    Load   v p -> tag 4 ++# encodeReg v ++# encodeReg p                 ++# 0
    Store  v p -> tag 5 ++# encodeReg v ++# encodeReg p                 ++# 0
    Jmp      p -> tag 6 ++# encodeReg p                                 ++# 0
    JmpZ   z d -> tag 7 ++# encodeReg z ++# encodeReg d                 ++# 0
    Out      v -> tag 8 ++# encodeReg v                                 ++# 0
    Halt       -> tag 9                                                 ++# 0
    where
    -- This is just for clarity, and to specify how many bits a tag should be.
    tag :: BitVector 4 -> BitVector 4
    tag x = x

-- We could have up to 16 regs (0 through 15),
--  but we're only using 4 for now.
encodeReg :: Register -> BitVector 4
encodeReg R1 = 1
encodeReg R2 = 2
encodeReg R3 = 3
encodeReg R4 = 4

decodeInstruction :: Word -> Instruction
decodeInstruction (Word val) = case tag of
    0 -> LoadIm a v
    1 -> Add    a b c
    2 -> Sub    a b c
    3 -> Mul    a b c
    4 -> Load   a b
    5 -> Store  a b
    6 -> Jmp    a
    7 -> JmpZ   a b
    8 -> Out    a
    9 -> Halt
    _ -> error "Undefined instruction"
    where
    tag = slice Nat.d63 Nat.d60 val
    a   = decodeReg $ slice Nat.d59 Nat.d56 val
    b   = decodeReg $ slice Nat.d55 Nat.d52 val
    c   = decodeReg $ slice Nat.d51 Nat.d48 val
    v   = unpack $ slice Nat.d55 Nat.d0  val

decodeReg :: BitVector 4 -> Register
decodeReg 1 = R1
decodeReg 2 = R2
decodeReg 3 = R3
decodeReg 4 = R4
decodeReg _ = error "Invalid register"


cycle :: (CPUState, RAM) -> (CPUState, RAM)
cycle (CPUState activity registers, ram) = case activity of
  LoadingInstruction -> (CPUState activity' registers', ram)
    where
      loadedWord = readRAM ram (pc registers)
      activity' = ExecutingInstruction (decodeInstruction loadedWord)
      registers' = registers {pc = increment (pc registers)}
  ExecutingInstruction instr -> case instr of
    LoadIm reg val -> (CPUState LoadingInstruction registers', ram)
      where
        registers' = writeRegister registers reg (zeroExtend val)
    Add a b d -> (CPUState LoadingInstruction registers', ram)
      where
        result = readRegister registers a + readRegister registers b
        registers' = writeRegister registers d result
    Sub a b d -> (CPUState LoadingInstruction registers', ram)
      where
        result = readRegister registers a - readRegister registers b
        registers' = writeRegister registers d result
    Mul a b d -> (CPUState LoadingInstruction registers', ram)
      where
        result = readRegister registers a * readRegister registers b
        registers' = writeRegister registers d result
    Load valReg ptrReg -> (CPUState (ReadingMemory ptr valReg) registers, ram)
      where
        ptr = Ptr(readRegister registers ptrReg)
    Store valReg ptrReg -> (CPUState (WritingMemory ptr val) registers, ram)
      where
        ptr = Ptr (readRegister registers ptrReg)
        val = Word (readRegister registers valReg)
    Jmp destReg -> (CPUState LoadingInstruction registers', ram)
      where
        pc' = readRegister registers destReg
        registers' = registers{pc = Ptr pc'}
    JmpZ zeroReg destReg -> (CPUState LoadingInstruction registers', ram)
      where
        pc' = readRegister registers destReg
        zeroVal = readRegister registers zeroReg
        registers' = if  zeroVal == 0
                    then registers{pc=Ptr pc'}
                    else registers
    Out reg -> (CPUState (Outputting output) registers, ram)
      where
        output = Output (readRegister registers reg)
    Halt -> (CPUState Halted registers, ram)
  ReadingMemory ptr reg -> (CPUState LoadingInstruction registers', ram)
    where
      Word ramValue = readRAM ram ptr
      registers' = writeRegister registers reg ramValue
  WritingMemory ptr val -> (CPUState LoadingInstruction registers, ram')
    where
      ram' = writeRAM ram ptr val
  Outputting _ -> (CPUState LoadingInstruction registers, ram)
  Halted -> (CPUState Halted registers, ram)


isHalted :: CPUState -> Bool
isHalted (CPUState Halted _) = True
isHalted _ = False

output :: CPUState -> Maybe Output
output (CPUState (Outputting output) _) = Just output
output _ = Nothing

getOutput :: (CPUState, RAM) -> (Bool, Maybe Output)
getOutput (state, _) = (isHalted state, output state)

cpuHardware :: (HiddenClockResetEnable dom) => CPUState -> RAM -> Signal dom (Bool, Maybe Output)
cpuHardware initialCPUState initialRam = outputSignal
  where
    systemState = register (initialCPUState, initialRam) systemState'
    systemState' = fmap cycle systemState
    outputSignal = fmap getOutput systemState'

defaultCPUState :: CPUState
defaultCPUState = CPUState LoadingInstruction (Registers 0 0 0 0 (Ptr 0))

simpleProgram :: Vec 7 Instruction
simpleProgram = 
    LoadIm R1 7 :>
    LoadIm R2 8 :>
    LoadIm R3 9 :>
    Out R1      :>
    Out R2      :>
    Out R3      :>
    Halt        :>
    Nil

simpleProgramMem :: Vec 64 Word
simpleProgramMem = fmap encodeInstruction simpleProgram ++ repeat (Word 0)

simpleProgramOutput :: [(Bool, Maybe Output)]
simpleProgramOutput = take 20 $ sample simpleProgramCPU
