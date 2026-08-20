module Main (main) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Project (findProjectRoot)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process (callProcess)

main :: IO ()
main = do
    root <- findProjectRoot =<< getCurrentDirectory
    compiler <- fromMaybe "cc" <$> lookupEnv "CC"
    let source = root </> "tests" </> "fixture.c"
        output = root </> "dist-newstyle" </> "fixtures"
        fixtures =
            [ ([], output </> "hello-pie")
            , (["-no-pie"], output </> "hello-exec")
            ]
    createDirectoryIfMissing True output
    forM_ fixtures $ \(flags, destination) -> do
        callProcess compiler $ ["-O0"] ++ flags ++ [source, "-o", destination]
        putStrLn $ "Created: " ++ destination
