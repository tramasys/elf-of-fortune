module Main (main) where

import Control.Exception (finally)
import Data.List (foldl')
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , findExecutable
  , getCurrentDirectory
  , removePathForcibly
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode, die, exitWith)
import System.FilePath ((</>), takeDirectory)
import System.Process (CreateProcess (env), createProcess, proc, waitForProcess)

main :: IO ()
main = do
  root <- findProjectRoot =<< getCurrentDirectory
  tool <- findExecutable "elf-of-fortune" >>= maybe (die "Cabal did not expose elf-of-fortune to the test suite") pure
  inherited <- getEnvironment
  let work = root </> "dist-newstyle" </> "test-work"
      fixtureDirectory = work </> "fixtures"
      resultDirectory = work </> "results"
      script = root </> "tests" </> "test.sh"
      overrides =
        [ ("ELF_OF_FORTUNE", tool)
        , ("FIXTURE_DIR", fixtureDirectory)
        , ("FIXTURE_PIE", fixtureDirectory </> "hello-pie")
        , ("FIXTURE_EXEC", fixtureDirectory </> "hello-exec")
        , ("TEST_RESULTS_DIR", resultDirectory)
        ]
      environment = foldl' setVariable inherited overrides
  removePathForcibly work
  createDirectoryIfMissing True work
  run script environment `finally` removePathForcibly work >>= exitWith

run :: FilePath -> [(String, String)] -> IO ExitCode
run script environment = do
  (_, _, _, process) <- createProcess (proc "bash" [script]) {env = Just environment}
  waitForProcess process

setVariable :: [(String, String)] -> (String, String) -> [(String, String)]
setVariable environment variable@(name, _) = variable : filter ((/= name) . fst) environment

findProjectRoot :: FilePath -> IO FilePath
findProjectRoot directory = do
  found <- doesFileExist $ directory </> "elf-of-fortune.cabal"
  if found
    then pure directory
    else do
      let parent = takeDirectory directory
      if parent == directory
        then die "cannot find elf-of-fortune.cabal"
        else findProjectRoot parent
