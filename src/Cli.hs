module Cli (
    Command (..),
    OutputMode (..),
    RunOptions,
    helpText,
    parseCommand,
    runDry,
    runInput,
    runInstruction,
    runOutputMode,
    runSeed,
    runWithoutAnimation,
) where

import Control.Monad ((<=<))
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Roulette (CrashInstruction, Rng, parseInstruction, seedToRng)

data Command
    = ShowHelp
    | Execute RunOptions

data OutputMode
    = DefaultOutput
    | OutputTo FilePath
    | ReplaceInPlace
    deriving (Eq)

data RunOptions = RunOptions
    { runInput :: FilePath
    , runOutputMode :: OutputMode
    , runSeed :: Maybe Rng
    , runWithoutAnimation :: Bool
    , runDry :: Bool
    , runInstruction :: Maybe CrashInstruction
    }

data PartialOptions = PartialOptions
    { partialOutput :: Maybe FilePath
    , partialInPlace :: Bool
    , partialSeed :: Maybe Rng
    , partialWithoutAnimation :: Bool
    , partialDry :: Bool
    , partialHelp :: Bool
    , partialInstruction :: Maybe CrashInstruction
    , partialInput :: Maybe FilePath
    }

parseCommand :: [String] -> Either String Command
parseCommand = finish <=< parse defaultOptions

helpText :: String
helpText =
    unlines
        [ "elf-of-fortune [options] <input>"
        , ""
        , "Options:"
        , "  -o, --output <path>    output path (default: <input>.doomed)"
        , "  --in-place             modify input after creating <input>.bak"
        , "  --seed <u64>           deterministic RNG seed"
        , "  --no-anim              disable carousel animation"
        , "  --dry-run              select everything but write nothing"
        , "  --help                 show this help"
        , ""
        , "The resulting output is intentionally corrupted and should crash immediately."
        ]

defaultOptions :: PartialOptions
defaultOptions = PartialOptions Nothing False Nothing False False False Nothing Nothing

parse :: PartialOptions -> [String] -> Either String PartialOptions
parse options arguments = case arguments of
    [] -> Right options
    argument : rest
        | argument == "-o" || argument == "--output" -> do
            ensureUnset (partialOutput options) "output path specified more than once"
            (value, remaining) <- needsValue argument rest
            parse options{partialOutput = Just value} remaining
        | argument == "--in-place" -> parse options{partialInPlace = True} rest
        | argument == "--seed" -> do
            ensureUnset (partialSeed options) "seed specified more than once"
            (value, remaining) <- needsValue argument rest
            seed <- seedToRng value
            parse options{partialSeed = Just seed} remaining
        | argument == "--no-anim" -> parse options{partialWithoutAnimation = True} rest
        | argument == "--dry-run" -> parse options{partialDry = True} rest
        | argument == "--help" -> parse options{partialHelp = True} rest
        | argument == "--instruction" -> do
            ensureUnset (partialInstruction options) "instruction specified more than once"
            (value, remaining) <- needsValue argument rest
            instruction <- parseInstruction value
            parse options{partialInstruction = Just instruction} remaining
        | "-" `isPrefixOf` argument -> Left $ "unknown option: " ++ argument
        | isJust $ partialInput options -> Left "exactly one input file is required"
        | not (null rest) -> Left "input must be the final argument"
        | otherwise -> parse options{partialInput = Just argument} rest

finish :: PartialOptions -> Either String Command
finish options
    | isJust (partialOutput options) && partialInPlace options =
        Left "--output and --in-place cannot be used together"
    | partialHelp options = Right ShowHelp
    | otherwise = Execute <$> runOptions options

runOptions :: PartialOptions -> Either String RunOptions
runOptions options = do
    input <- maybe (Left "missing input file") Right $ partialInput options
    pure
        RunOptions
            { runInput = input
            , runOutputMode = outputMode options
            , runSeed = partialSeed options
            , runWithoutAnimation = partialWithoutAnimation options
            , runDry = partialDry options
            , runInstruction = partialInstruction options
            }

outputMode :: PartialOptions -> OutputMode
outputMode options
    | partialInPlace options = ReplaceInPlace
    | otherwise = maybe DefaultOutput OutputTo $ partialOutput options

needsValue :: String -> [String] -> Either String (String, [String])
needsValue option arguments = case arguments of
    [] -> Left $ option ++ " requires a value"
    value : rest -> Right (value, rest)

ensureUnset :: Maybe value -> String -> Either String ()
ensureUnset current message = maybe (Right ()) (const $ Left message) current
