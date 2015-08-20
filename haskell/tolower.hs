import System.Directory
import Control.Monad(filterM)
import Data.Char(toLower)
import Data.List

main :: IO ()
main = getCurrentDirectory >>= renameAndRecurse

getSubdirectories :: IO [FilePath]
getSubdirectories = do
	files <- getCurrentDirectory >>= getDirectoryContents
	filterM doesDirectoryExist files

renameAndRecurse :: FilePath -> IO ()
renameAndRecurse dir = do
	getCurrentDirectory >>= \d -> (putStrLn ("Current directory: " ++ d))
	setCurrentDirectory dir 
	fileList' <- getCurrentDirectory >>= getDirectoryContents 
	dirList' <- getSubdirectories
	let fileList = fileList' \\ dirList'
	let dirList = dirList' \\ [".",".."]
	mapM (\f -> (putStrLn ("Renaming " ++ f)) >> renameFile f (map toLower f)) fileList
	mapM (\d -> (putStrLn ("Recursing to " ++ d ++ "...")) >> renameAndRecurse d) dirList
	mapM (\d -> (putStrLn ("Renaming " ++ d)) >> renameDirectory d (map toLower d)) dirList
	putStrLn $ "Done with " ++ dir
	setCurrentDirectory ".."
	getCurrentDirectory >>= \d -> (putStrLn ("Returned to directory: " ++ d))
