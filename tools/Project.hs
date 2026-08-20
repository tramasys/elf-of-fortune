module Project (findProjectRoot) where

import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))

findProjectRoot :: FilePath -> IO FilePath
findProjectRoot directory = do
    found <- doesFileExist $ directory </> "elf-of-fortune.cabal"
    if found
        then pure directory
        else ascend $ takeDirectory directory
  where
    ascend parent
        | parent == directory = die "cannot find elf-of-fortune.cabal"
        | otherwise = findProjectRoot parent
