import System.Directory
import Control.Monad
import Data.List

getFiles :: [FilePath] -> IO [FilePath]
getFiles = filterM (doesFileExist)

getDirectories :: [FilePath] -> IO [FilePath]
getDirectories = filterM (doesDirectoryExist)

prependParentDir :: FilePath -> FilePath -> FilePath
prependParentDir p c = p ++ ("/" ++ c)

removeSpecialDirs :: [FilePath] -> [FilePath]
removeSpecialDirs = filter (\d -> not $ d `elem` [".", ".."])

makeFullPathAndSort :: FilePath -> [FilePath] -> [FilePath]
makeFullPathAndSort p = sort . map (prependParentDir p)

processDir :: FilePath -> IO ()
processDir d = do
    printDir d
    contents <- getDirectoryContents d
    directories <- getDirectories $ makeFullPathAndSort d $ removeSpecialDirs contents 
    mapM_ processDir directories

printDir :: FilePath -> IO () 
printDir d = do
    putStrLn ""
    putStrLn $ "current directory:   " ++ d
    contents <- getDirectoryContents d
    files <- getFiles $ makeFullPathAndSort d contents
    directories <- getDirectories $ makeFullPathAndSort d $ removeSpecialDirs contents
    let listing = (map ("             file:   " ++) files) ++ (map ("        directory:   " ++) directories)
    mapM_ putStrLn $ listing

main = do
    currentDirectory <- getCurrentDirectory
    processDir currentDirectory
