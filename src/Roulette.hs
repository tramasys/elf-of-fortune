module Roulette (
    CrashInstruction,
    Rng,
    chooseInstruction,
    crashDescription,
    crashName,
    crashPayload,
    crashSignal,
    instructionAt,
    instructionIndex,
    parseInstruction,
    rngFromWord64,
    seedToRng,
    uniformIndex,
) where

import Data.List (find, foldl')
import Data.Word (Word64, Word8)

data CrashInstruction
    = UD2
    | INT3
    | HLT
    | ICEBP
    | UD1
    | LockNop
    deriving (Bounded, Enum, Eq, Show)

newtype Rng = Rng Int
    deriving (Eq, Show)

data InstructionSpec = InstructionSpec
    { specName :: String
    , specPayload :: [Word8]
    , specDescription :: String
    , specSignal :: String
    }

crashName :: CrashInstruction -> String
crashName = specName . instructionSpec

crashPayload :: CrashInstruction -> [Word8]
crashPayload = specPayload . instructionSpec

crashDescription :: CrashInstruction -> String
crashDescription = specDescription . instructionSpec

crashSignal :: CrashInstruction -> String
crashSignal = specSignal . instructionSpec

instructionSpec :: CrashInstruction -> InstructionSpec
instructionSpec instruction = case instruction of
    UD2 -> InstructionSpec "UD2" [0x0f, 0x0b] "invalid-opcode exception" "SIGILL"
    INT3 -> InstructionSpec "INT3" [0xcc] "breakpoint exception" "SIGTRAP"
    HLT -> InstructionSpec "HLT" [0xf4] "privileged instruction from CPL3" "SIGSEGV"
    ICEBP -> InstructionSpec "ICEBP" [0xf1] "debug exception" "SIGTRAP"
    UD1 -> InstructionSpec "UD1" [0x0f, 0xb9, 0xc0] "undefined-instruction exception" "SIGILL"
    LockNop -> InstructionSpec "LOCK NOP" [0xf0, 0x90] "invalid LOCK prefix usage" "SIGILL"

parseInstruction :: String -> Either String CrashInstruction
parseInstruction name =
    maybe (Left "unknown instruction for --instruction") Right $
        find ((== name) . crashName) allInstructions

chooseInstruction :: Rng -> Maybe CrashInstruction -> Either String (CrashInstruction, Rng)
chooseInstruction rng forced = case forced of
    Just instruction -> Right (instruction, rng)
    Nothing -> do
        (index, next) <- uniformIndex instructionCount rng
        pure (instructionAt index, next)

instructionAt :: Int -> CrashInstruction
instructionAt = toEnum . (`mod` instructionCount)

instructionIndex :: CrashInstruction -> Int
instructionIndex = fromEnum

seedToRng :: String -> Either String Rng
seedToRng text
    | null text || not (all isAsciiDigit text) = Left invalidSeed
    | fitsInWord64 text = Right $ rngFromDigits text
    | otherwise = Left invalidSeed

rngFromWord64 :: Word64 -> Rng
rngFromWord64 = rngFromDigits . show

uniformIndex :: Int -> Rng -> Either String (Int, Rng)
uniformIndex bound rng
    | bound <= 0 = Left "internal invalid random bound"
    | otherwise = Right (value `mod` bound, next)
  where
    next@(Rng value) = nextRng rng

allInstructions :: [CrashInstruction]
allInstructions = [minBound .. maxBound]

instructionCount :: Int
instructionCount = fromEnum (maxBound :: CrashInstruction) + 1

rngFromDigits :: String -> Rng
rngFromDigits digits = Rng $ nonzero $ foldl' step 0 digits
  where
    step state character = (state `mod` seedFoldBound) * 10 + fromEnum character - fromEnum '0'
    nonzero 0 = 1
    nonzero state = state

nextRng :: Rng -> Rng
nextRng (Rng state) = Rng $ if candidate <= 0 then candidate + rngModulus else candidate
  where
    high = state `div` 127773
    low = state `mod` 127773
    candidate = 16807 * low - 2836 * high

isAsciiDigit :: Char -> Bool
isAsciiDigit = (`elem` ['0' .. '9'])

fitsInWord64 :: String -> Bool
fitsInWord64 digits = case dropWhile (== '0') digits of
    [] -> True
    significant ->
        length significant < length maximumWord64
            || (length significant == length maximumWord64 && significant <= maximumWord64)

maximumWord64 :: String
maximumWord64 = show (maxBound :: Word64)

invalidSeed :: String
invalidSeed = "invalid --seed value (expected u64)"

seedFoldBound :: Int
seedFoldBound = 214748362

rngModulus :: Int
rngModulus = 2147483647
