module Main (main) where

import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)
import System.Process (callProcess)

main :: IO ()
main = do
  root <- findProjectRoot =<< getCurrentDirectory
  compiler <- maybe "cc" id <$> lookupEnv "CC"
  let source = root </> "tests" </> "fixture.c"
      output = root </> "dist-newstyle" </> "fixtures"
      pie = output </> "hello-pie"
      executable = output </> "hello-exec"
  createDirectoryIfMissing True output
  callProcess compiler ["-O0", source, "-o", pie]
  callProcess compiler ["-O0", "-no-pie", source, "-o", executable]
  putStrLn $ "Created: " ++ pie
  putStrLn $ "Created: " ++ executable

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
