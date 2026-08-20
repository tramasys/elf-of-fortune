module Main (main) where

import Cli (
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
 )
import Control.Exception (Exception, IOException, catch, onException, throwIO)
import Control.Monad (unless, when)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word64)
import Elf (PatchPlan, applyPatch, planPatch)
import Roulette (CrashInstruction, Rng, chooseInstruction, rngFromWord64)
import System.Directory (doesPathExist, removeFile, renameFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (
    BufferMode (LineBuffering),
    IOMode (ReadMode),
    hClose,
    hFlush,
    hIsTerminalDevice,
    hPutStrLn,
    hSetBuffering,
    hSetEncoding,
    openBinaryTempFile,
    stderr,
    stdout,
    utf8,
    withBinaryFile,
 )
import System.IO.Error (ioeGetErrorString)
import System.Posix.Files (deviceID, fileID, fileMode, getFileStatus, setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Time (epochTime)
import System.Posix.Types (FileMode)
import Terminal (animateWheel, showStaticWheel, showSummary)

data AppError = AppError Int String
    deriving (Show)

instance Exception AppError

main :: IO ()
main = do
    configureTerminal
    terminal <- hIsTerminalDevice stdout
    arguments <- getArgs
    run terminal arguments `catch` handleError terminal

configureTerminal :: IO ()
configureTerminal = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetBuffering stdout LineBuffering

handleError :: Bool -> AppError -> IO ()
handleError terminal (AppError code message) = do
    when terminal $ putStr "\ESC[0m"
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure code

run :: Bool -> [String] -> IO ()
run terminal arguments = do
    when (any containsNewline arguments) $
        throwIO $
            AppError 2 "newlines in command-line arguments are unsupported"
    command <- fromEither $ parseCommand arguments
    case command of
        ShowHelp -> putStr helpText
        Execute options -> execute terminal options

execute :: Bool -> RunOptions -> IO ()
execute terminal options = do
    let input = runInput options
        destination = destinationPath (runOutputMode options) input
    bytes <- ioOrError ("cannot read input '" ++ input ++ "'") $ BS.readFile input
    status <- ioOrError ("cannot read input '" ++ input ++ "'") $ getFileStatus input
    rng <- maybe randomRng pure $ runSeed options
    sameInput <- sameExistingFile input destination
    when (runOutputMode options /= ReplaceInPlace && sameInput) $
        throwIO $
            AppError 1 "refusing to overwrite input without --in-place"
    (instruction, patchRng) <- fromEither $ chooseInstruction rng $ runInstruction options
    presentWheel terminal (runWithoutAnimation options) instruction
    plan <- fromEither $ planPatch bytes instruction patchRng
    showSummary plan input destination $ runDry options
    unless (runDry options) $
        writeResult options input destination (fileMode status) bytes plan

writeResult :: RunOptions -> FilePath -> FilePath -> FileMode -> BS.ByteString -> PatchPlan -> IO ()
writeResult options input destination mode bytes plan = do
    when (runOutputMode options == ReplaceInPlace) $ do
        let backup = input ++ ".bak"
        ioOrError ("cannot create backup '" ++ backup ++ "'") $ atomicWrite backup mode bytes
    let patched = applyPatch bytes plan
    ioOrError ("cannot create output '" ++ destination ++ "'") $ atomicWrite destination mode patched
    putStrLn $ "Created: " ++ destination

presentWheel :: Bool -> Bool -> CrashInstruction -> IO ()
presentWheel terminal withoutAnimation instruction
    | withoutAnimation || not terminal = showStaticWheel instruction terminal
    | otherwise = animateWheel instruction

destinationPath :: OutputMode -> FilePath -> FilePath
destinationPath mode input = case mode of
    DefaultOutput -> input ++ ".doomed"
    OutputTo output -> output
    ReplaceInPlace -> input

sameExistingFile :: FilePath -> FilePath -> IO Bool
sameExistingFile left right
    | left == right = pure True
    | otherwise = do
        exists <- doesPathExist right
        if exists then catch compareFiles ignoreStatusError else pure False
  where
    compareFiles = do
        a <- getFileStatus left
        b <- getFileStatus right
        pure $ deviceID a == deviceID b && fileID a == fileID b

    ignoreStatusError :: IOException -> IO Bool
    ignoreStatusError _ = pure False

atomicWrite :: FilePath -> FileMode -> BS.ByteString -> IO ()
atomicWrite path mode bytes = do
    let directory = takeDirectory path
        template = takeFileName path ++ ".tmp"
    (temporary, handle) <- openBinaryTempFile directory template
    let cleanup = do
            ignoreIOException $ hClose handle
            ignoreIOException $ removeFile temporary
        write = do
            BS.hPut handle bytes
            hFlush handle
            hClose handle
            setFileMode temporary mode
            renameFile temporary path
    write `onException` cleanup

ignoreIOException :: IO () -> IO ()
ignoreIOException action = catch action ignore
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()

ioOrError :: String -> IO value -> IO value
ioOrError action operation = catch operation handler
  where
    handler :: IOException -> IO value
    handler exception = throwIO $ AppError 1 $ action ++ ": " ++ ioeGetErrorString exception

fromEither :: Either String value -> IO value
fromEither = either (throwIO . AppError 1) pure

randomRng :: IO Rng
randomRng = rngFromWord64 <$> entropySeed

entropySeed :: IO Word64
entropySeed = catch fromUrandom fallback
  where
    fromUrandom = do
        bytes <- withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 8)
        if BS.length bytes == 8 then pure (decodeWord64 bytes) else fallbackSeed

    fallback :: IOException -> IO Word64
    fallback _ = fallbackSeed

    fallbackSeed = do
        process <- getProcessID
        now <- epochTime
        pure $ fromIntegral process `shiftL` 32 .|. fromIntegral (fromEnum now)

decodeWord64 :: BS.ByteString -> Word64
decodeWord64 = fst . BS.foldl' addByte (0, 0)
  where
    addByte (value, shift) byte =
        (value .|. (fromIntegral byte `shiftL` shift), shift + 8)

containsNewline :: String -> Bool
containsNewline = any (`elem` ['\n', '\r'])
