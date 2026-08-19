module Terminal
  ( animateWheel
  , showStaticWheel
  , showSummary
  ) where

import Control.Concurrent (threadDelay)
import Data.List (intercalate)
import Elf (PatchPlan (..), regionSource)
import Roulette (CrashInstruction (..), allInstructions, instructionIndex)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

animateWheel :: CrashInstruction -> IO ()
animateWheel instruction = mapM_ frame [0 .. 23]
  where
    selected = instructionIndex instruction
    frame index = do
      renderWheel (selected + index + 1) True (index > 0)
      if index < 23
        then threadDelay $ frameDelay index * 1000
        else pure ()

showStaticWheel :: CrashInstruction -> Bool -> IO ()
showStaticWheel instruction ansi = renderWheel (instructionIndex instruction) ansi False

renderWheel :: Int -> Bool -> Bool -> IO ()
renderWheel center ansi rewind = do
  if rewind then putStr $ "\ESC[" ++ show wheelHeight ++ "A\ESC[J" else pure ()
  putStrLn $ centeredText "ELF OF FORTUNE"
  putStrLn ""
  putStrLn $ replicate wheelCenter ' ' ++ "│"
  putStrLn ""
  putStrLn $ wheelLine center ansi
  putStrLn ""
  putStrLn $ replicate (wheelCenter - 1) ' ' ++ "🐈"
  hFlush stdout

wheelLine :: Int -> Bool -> String
wheelLine center ansi =
  let left = wheelName (center - 2) ++ "   " ++ wheelName (center - 1) ++ "  "
      selected = highlightedName center ansi
      selectedWidth = length (wheelName center) + 4
      indentation = max 0 $ wheelCenter - length left - selectedWidth `div` 2
   in replicate indentation ' '
        ++ left
        ++ selected
        ++ "  "
        ++ wheelName (center + 1)
        ++ "   "
        ++ wheelName (center + 2)

centeredText :: String -> String
centeredText text = replicate indentation ' ' ++ text
  where
    indentation = max 0 $ wheelCenter - length text `div` 2

wheelCenter :: Int
wheelCenter = 25

wheelHeight :: Int
wheelHeight = 7

wheelName :: Int -> String
wheelName index = crashName $ allInstructions !! wheelIndex index

highlightedName :: Int -> Bool -> String
highlightedName index ansi
  | ansi = "[\ESC[7m " ++ wheelName index ++ " \ESC[0m]"
  | otherwise = "[ " ++ wheelName index ++ " ]"

wheelIndex :: Int -> Int
wheelIndex index = index `mod` length allInstructions

frameDelay :: Int -> Int
frameDelay frame
  | frame < 12 = 75
  | frame < 18 = 135
  | frame < 22 = 225
  | otherwise = 390

showSummary :: PatchPlan -> FilePath -> FilePath -> Bool -> IO ()
showSummary plan input destination dryRun = do
  let instruction = patchInstruction plan
  putStrLn ""
  putStrLn $ centeredText "The cat has made its decision!"
  putStrLn ""
  putStrLn $ "Victim:       " ++ input
  putStrLn $ "Instruction:  " ++ crashName instruction
  putStrLn $ "Bytes:        " ++ payloadHex instruction
  putStrLn $ "Effect:       " ++ crashDescription instruction
  putStrLn $ "Signal:       " ++ crashSignal instruction
  putStrLn $ "Region:       " ++ regionSource (patchRegion plan)
  printf "File offset:  0x%08x\n" (patchOffset plan)
  printf "Address:      0x%016x\n" (patchAddress plan)
  printf "Old entry:    0x%016x\n" (patchOldEntry plan)
  printf "New entry:    0x%016x\n" (patchAddress plan)
  putStrLn ""
  if dryRun
    then putStrLn "Dry run: no file was written."
    else putStrLn $ "Destination: " ++ destination

payloadHex :: CrashInstruction -> String
payloadHex = intercalate " " . map (printf "%02x") . crashPayload
