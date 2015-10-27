import Data.List
import Data.Char
import System.Environment (getArgs)

main :: IO ()
main = do
  tiles <- fmap head $ getArgs
  dictStr  <- fmap lines $ readFile "/usr/share/dict/cracklib-small"
  let dict = map (filter isAlpha) dictStr
  let matches = case '*' `elem` tiles of
                  False -> filter (flip isSubsequenceOf tiles) $ dict
                  True  -> filter (\w -> 1 < length w) . tail . nub . concat $ do
                                        c <- ['a'..'z']
                                        return $ filter (flip isSubsequenceOf (c:tiles)) $ dict
  print $ sortBy (\a b -> compare (length a) (length b)) matches
