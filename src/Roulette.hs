module Roulette
  ( CrashInstruction (..)
  , allInstructions
  , chooseInstruction
  , instructionIndex
  , rngBounded
  , seedToState
  ) where

import Data.Char (digitToInt, isDigit)
import Data.Word (Word64, Word8)
import Text.Read (readMaybe)

data CrashInstruction = CrashInstruction
  { crashName :: String
  , crashPayload :: [Word8]
  , crashDescription :: String
  , crashSignal :: String
  }
  deriving (Eq, Show)

allInstructions :: [CrashInstruction]
allInstructions =
  [ CrashInstruction "UD2" [0x0f, 0x0b] "invalid-opcode exception" "SIGILL"
  , CrashInstruction "INT3" [0xcc] "breakpoint exception" "SIGTRAP"
  , CrashInstruction "HLT" [0xf4] "privileged instruction from CPL3" "SIGSEGV"
  , CrashInstruction "ICEBP" [0xf1] "debug exception" "SIGTRAP"
  , CrashInstruction "UD1" [0x0f, 0xb9, 0xc0] "undefined-instruction exception" "SIGILL"
  , CrashInstruction "LOCK NOP" [0xf0, 0x90] "invalid LOCK prefix usage" "SIGILL"
  ]

seedToState :: String -> Either String Int
seedToState text = do
  _ <- parseSeed text
  let state = foldl step 0 text
  pure $ if state == 0 then 1 else state
  where
    step state character = (state `mod` 214748362) * 10 + digitToInt character

parseSeed :: String -> Either String Word64
parseSeed text
  | null text || not (all isDigit text) = Left invalidSeed
  | otherwise = maybe (Left invalidSeed) Right (readMaybe text)
  where
    invalidSeed = "invalid --seed value (expected u64)"

chooseInstruction :: Int -> Maybe String -> Either String (CrashInstruction, Int)
chooseInstruction state forced = case forced of
  Just wanted -> case filter ((== wanted) . crashName) allInstructions of
    instruction : _ -> Right (instruction, state)
    [] -> Left "unknown instruction for --instruction"
  Nothing -> do
    (next, index) <- rngBounded state (length allInstructions)
    case drop index allInstructions of
      instruction : _ -> Right (instruction, next)
      [] -> Left "internal crash-instruction index out of range"

instructionIndex :: CrashInstruction -> Int
instructionIndex instruction = go 0 allInstructions
  where
    go index (candidate : rest)
      | crashName candidate == crashName instruction = index
      | otherwise = go (index + 1) rest
    go _ [] = error "internal crash instruction is not on wheel"

rngBounded :: Int -> Int -> Either String (Int, Int)
rngBounded state bound
  | bound <= 0 = Left "internal invalid random bound"
  | otherwise =
      let next = rngNext state
       in Right (next, next `mod` bound)

rngNext :: Int -> Int
rngNext state =
  let high = state `div` 127773
      low = state `mod` 127773
      value = 16807 * low - 2836 * high
   in if value <= 0 then value + 2147483647 else value
