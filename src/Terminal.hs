module Terminal (
    animateWheel,
    showStaticWheel,
    showSummary,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Elf (
    PatchPlan,
    patchAddress,
    patchInstruction,
    patchOffset,
    patchOldEntry,
    patchRegion,
    regionSource,
 )
import Roulette (
    CrashInstruction,
    crashDescription,
    crashName,
    crashPayload,
    crashSignal,
    instructionAt,
    instructionIndex,
 )
import System.IO (hFlush, stdout)
import Text.Printf (printf)

animateWheel :: CrashInstruction -> IO ()
animateWheel instruction = forM_ [0 .. finalFrame] renderFrame
  where
    selected = instructionIndex instruction
    renderFrame frame = do
        renderWheel (selected + frame + 1) True $ frame > 0
        when (frame < finalFrame) $ threadDelay $ frameDelay frame * 1000

showStaticWheel :: CrashInstruction -> Bool -> IO ()
showStaticWheel instruction ansi = renderWheel (instructionIndex instruction) ansi False

renderWheel :: Int -> Bool -> Bool -> IO ()
renderWheel center ansi rewind = do
    let image = wheelFrame center ansi
    when rewind $ putStr $ cursorUp (length image) ++ "\ESC[J"
    putStr $ unlines image
    hFlush stdout

wheelFrame :: Int -> Bool -> [String]
wheelFrame center ansi =
    [ centeredText "ELF OF FORTUNE"
    , ""
    , centeredText "│"
    , ""
    , wheelLine center ansi
    , ""
    , centeredWithWidth 2 "🐈"
    ]

wheelLine :: Int -> Bool -> String
wheelLine center ansi =
    replicate indentation ' '
        ++ left
        ++ highlightedName center ansi
        ++ "  "
        ++ wheelName (center + 1)
        ++ "   "
        ++ wheelName (center + 2)
  where
    left = wheelName (center - 2) ++ "   " ++ wheelName (center - 1) ++ "  "
    selectedWidth = length (wheelName center) + 4
    indentation = max 0 $ wheelCenter - length left - selectedWidth `div` 2

highlightedName :: Int -> Bool -> String
highlightedName index ansi
    | ansi = "[\ESC[7m " ++ wheelName index ++ " \ESC[0m]"
    | otherwise = "[ " ++ wheelName index ++ " ]"

wheelName :: Int -> String
wheelName = crashName . instructionAt

centeredText :: String -> String
centeredText text = centeredWithWidth (length text) text

centeredWithWidth :: Int -> String -> String
centeredWithWidth width text = replicate indentation ' ' ++ text
  where
    indentation = max 0 $ wheelCenter - width `div` 2

cursorUp :: Int -> String
cursorUp linesToMove = "\ESC[" ++ show linesToMove ++ "A"

frameDelay :: Int -> Int
frameDelay frame
    | frame < 12 = 75
    | frame < 18 = 135
    | frame < 22 = 225
    | otherwise = 390

showSummary :: PatchPlan -> FilePath -> FilePath -> Bool -> IO ()
showSummary plan input destination dryRun = putStr $ unlines linesToShow
  where
    instruction = patchInstruction plan
    linesToShow =
        [ ""
        , centeredText "The cat has made its decision!"
        , ""
        , "Victim:       " ++ input
        , "Instruction:  " ++ crashName instruction
        , "Bytes:        " ++ payloadHex instruction
        , "Effect:       " ++ crashDescription instruction
        , "Signal:       " ++ crashSignal instruction
        , "Region:       " ++ regionSource (patchRegion plan)
        , printf "File offset:  0x%08x" $ patchOffset plan
        , printf "Address:      0x%016x" $ patchAddress plan
        , printf "Old entry:    0x%016x" $ patchOldEntry plan
        , printf "New entry:    0x%016x" $ patchAddress plan
        , ""
        , if dryRun then "Dry run: no file was written." else "Destination: " ++ destination
        ]

payloadHex :: CrashInstruction -> String
payloadHex = unwords . map (printf "%02x") . crashPayload

wheelCenter :: Int
wheelCenter = 25

finalFrame :: Int
finalFrame = 23
