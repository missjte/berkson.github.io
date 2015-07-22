import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription as PD

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (find, isPrefixOf)
import System.Directory (copyFile, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { postBuild = copyBinary }

copyBinary :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyBinary args buildFlags pkgDesc buildInfo = do
  let exe = find ("blog" `isPrefixOf`) $ map exeName (executables pkgDesc)

  when (isJust exe) $ do
    let binary = fromJust exe

    putStrLn $ "Copying executable '" ++ binary ++ "' to current directory..."
    copyFile (buildDir buildInfo </> "blog" </> binary) binary

    e <- doesDirectoryExist "generated"
    when e$ putStrLn "Cleaning previous generated content..."
    when e$ removeDirectoryRecursive "generated"

    putStrLn "Cleaning build directory..."
    removeDirectoryRecursive "dist"
