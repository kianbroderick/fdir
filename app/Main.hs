module Main where

import Data.Char (toLower)
import Distribution.Simple.Utils (safeHead)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.IO
import System.Posix.Files (rename)

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace match r (x : xs)
  | x == match = r : replace match r xs
  | otherwise = x : replace match r xs

replaceDash :: String -> String
replaceDash = replace '-' ' '

replaceUnderscore :: String -> String
replaceUnderscore = replace '_' ' '

format :: FilePath -> FilePath
format = replaceDash . replaceUnderscore . fmap toLower

formatFiles :: [FilePath] -> IO ()
formatFiles [] = do
  putStrLn "Done"
  return ()
formatFiles (x : xs) =
  let formatted = format x
   in if formatted == x
        then formatFiles xs
        else do
          putStrLn $ "Renaming " ++ x ++ " to " ++ formatted
          rename x formatted
          formatFiles xs

main :: IO ()
main = do
  args <- fmap head getArgs
  putStrLn ("Formatting all files in " ++ show args)
  contents <- listDirectory args
  let contents' = fmap ((args ++ "/") ++) contents
  formatFiles contents'
