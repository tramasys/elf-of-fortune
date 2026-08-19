module Main (main) where

import Control.Exception (Exception, IOException, catch, onException, throwIO)
import Control.Monad ((<=<), when)
import qualified Data.ByteString as BS
import Data.Bits ((.|.), shiftL)
import Data.List (isPrefixOf)
import Data.Word (Word64)
import Elf (applyPatch, planPatch)
import Roulette (CrashInstruction, chooseInstruction, seedToState)
import System.Directory (doesPathExist, removeFile, renameFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, takeFileName)
import System.IO
  ( BufferMode (LineBuffering)
  , IOMode (ReadMode)
  , hClose
  , hFlush
  , hIsTerminalDevice
  , hPutStrLn
  , hSetBuffering
  , hSetEncoding
  , openBinaryTempFile
  , stderr
  , stdout
  , utf8
  , withBinaryFile
  )
import System.IO.Error (ioeGetErrorString)
import System.Posix.Files (deviceID, fileID, fileMode, getFileStatus, setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Time (epochTime)
import System.Posix.Types (FileMode)
import Terminal (animateWheel, showStaticWheel, showSummary)

data Options = Options
  { optionOutput :: Maybe FilePath
  , optionInPlace :: Bool
  , optionSeed :: Maybe String
  , optionNoAnimation :: Bool
  , optionDryRun :: Bool
  , optionHelp :: Bool
  , optionInstruction :: Maybe String
  , optionInput :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options Nothing False Nothing False False False Nothing Nothing

data AppError = AppError Int String
  deriving (Show)

instance Exception AppError

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering
  terminal <- hIsTerminalDevice stdout
  arguments <- getArgs
  run terminal arguments `catch` handleError terminal

handleError :: Bool -> AppError -> IO ()
handleError terminal (AppError code message) = do
  when terminal $ putStr "\ESC[0m"
  hPutStrLn stderr $ "error: " ++ message
  exitWith $ ExitFailure code

run :: Bool -> [String] -> IO ()
run terminal arguments = do
  when (any containsNewline arguments) $ throwIO $ AppError 2 "newlines in command-line arguments are unsupported"
  options <- either (throwIO . AppError 1) pure $ parseArgs arguments
  if optionHelp options
    then putStr helpText
    else execute terminal options

execute :: Bool -> Options -> IO ()
execute terminal options = do
  input <- maybe (throwIO $ AppError 1 "missing input file") pure $ optionInput options
  bytes <- ioOrError ("cannot read input '" ++ input ++ "'") $ BS.readFile input
  status <- ioOrError ("cannot read input '" ++ input ++ "'") $ getFileStatus input
  state <- case optionSeed options of
    Just seed -> either (throwIO . AppError 1) pure $ seedToState seed
    Nothing -> entropySeed >>= either (throwIO . AppError 1) pure . seedToState . show
  let destination = destinationPath options input
  sameInput <- sameExistingFile input destination
  when (not (optionInPlace options) && sameInput) $ throwIO $ AppError 1 "refusing to overwrite input without --in-place"
  (instruction, patchState) <- either (throwIO . AppError 1) pure $ chooseInstruction state (optionInstruction options)
  presentWheel terminal options instruction
  plan <- either (throwIO . AppError 1) pure $ planPatch bytes instruction patchState
  showSummary plan input destination (optionDryRun options)
  if optionDryRun options
    then pure ()
    else do
      let mode = fileMode status
          patched = applyPatch bytes plan
      when (optionInPlace options) $ do
        let backup = input ++ ".bak"
        ioOrError ("cannot create backup '" ++ backup ++ "'") $ atomicWrite backup mode bytes
      ioOrError ("cannot create output '" ++ destination ++ "'") $ atomicWrite destination mode patched
      putStrLn $ "Created: " ++ destination

presentWheel :: Bool -> Options -> CrashInstruction -> IO ()
presentWheel terminal options instruction
  | optionNoAnimation options || not terminal = showStaticWheel instruction terminal
  | otherwise = animateWheel instruction

destinationPath :: Options -> FilePath -> FilePath
destinationPath options input
  | optionInPlace options = input
  | otherwise = maybe (input ++ ".doomed") id $ optionOutput options

sameExistingFile :: FilePath -> FilePath -> IO Bool
sameExistingFile left right
  | left == right = pure True
  | otherwise = do
      exists <- doesPathExist right
      if not exists
        then pure False
        else catch compareFiles ignoreStatusError
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

entropySeed :: IO Word64
entropySeed = catch fromUrandom fallback
  where
    fromUrandom = do
      bytes <- withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 8)
      if BS.length bytes < 8
        then fallbackSeed
        else
          let addByte value index = value .|. (fromIntegral (BS.index bytes index) `shiftL` (index * 8))
           in pure $ foldl addByte 0 [0 .. 7]
    fallback :: IOException -> IO Word64
    fallback _ = fallbackSeed
    fallbackSeed = do
      process <- getProcessID
      now <- epochTime
      pure $ fromIntegral process `shiftL` 32 .|. fromIntegral (fromEnum now)

containsNewline :: String -> Bool
containsNewline text = '\n' `elem` text || '\r' `elem` text

parseArgs :: [String] -> Either String Options
parseArgs = finish <=< go defaultOptions
  where
    go options [] = Right options
    go options (argument : rest)
      | argument == "-o" || argument == "--output" = do
          whenEither (optionOutput options /= Nothing) "output path specified more than once"
          (value, remaining) <- needsValue argument rest
          go options {optionOutput = Just value} remaining
      | argument == "--in-place" = go options {optionInPlace = True} rest
      | argument == "--seed" = do
          whenEither (optionSeed options /= Nothing) "seed specified more than once"
          (value, remaining) <- needsValue argument rest
          _ <- seedToState value
          go options {optionSeed = Just value} remaining
      | argument == "--no-anim" = go options {optionNoAnimation = True} rest
      | argument == "--dry-run" = go options {optionDryRun = True} rest
      | argument == "--help" = go options {optionHelp = True} rest
      | argument == "--instruction" = do
          whenEither (optionInstruction options /= Nothing) "instruction specified more than once"
          (value, remaining) <- needsValue argument rest
          _ <- chooseInstruction 1 (Just value)
          go options {optionInstruction = Just value} remaining
      | "-" `isPrefixOf` argument = Left $ "unknown option: " ++ argument
      | optionInput options /= Nothing = Left "exactly one input file is required"
      | not (null rest) = Left "input must be the final argument"
      | otherwise = go options {optionInput = Just argument} rest

    finish options
      | optionOutput options /= Nothing && optionInPlace options = Left "--output and --in-place cannot be used together"
      | otherwise = Right options

    needsValue option [] = Left $ option ++ " requires a value"
    needsValue _ (value : rest) = Right (value, rest)

    whenEither condition message = if condition then Left message else Right ()

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
