import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup

import           Control.Monad                      (when)
import           Data.List                          (find, isPrefixOf)
import           Data.Maybe                         (fromJust, isJust)
import           System.Directory                   (copyFile,
                                                     doesDirectoryExist,
                                                     removeDirectoryRecursive)
import           System.FilePath                    ((</>))

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { postBuild = copyBinary }

copyBinary :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyBinary args buildFlags pkgDesc buildInfo = do
  let exe = find ("blog" `isPrefixOf`) $ map exeName (executables pkgDesc)

  when (isJust exe) $ do
    let binary = fromJust exe

    putStrLn $ "Copying executable '" ++ binary ++ "' to current directory..."
    copyFile (buildDir buildInfo </> "blog" </> binary) binary

    g <- doesDirectoryExist "generated"
    when g$ putStrLn "Cleaning previous generated content..."
    when g$ removeDirectoryRecursive "generated"

    b <- doesDirectoryExist "disc"
    when b$ putStrLn "Cleaning build directory..."
    when b$ removeDirectoryRecursive "disc"
