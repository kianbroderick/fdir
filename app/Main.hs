module Main where

import Data.Char (toLower, toUpper)
import Options.Applicative
import System.Directory (doesDirectoryExist, listDirectory, setCurrentDirectory, withCurrentDirectory)
import System.Posix.Files (rename)

type Formatter = FilePath -> FilePath

data Style
  = SnakeCase
  | CamelCase
  | PascalCase
  | KebabCase

data Separator = Dash | Underscore | Space deriving (Eq, Show, Read)

data Input = Input
  { nameInput :: FilePath,
    lowercaseInput :: Bool,
    uppercaseInput :: Bool,
    recursiveInput :: Bool,
    separatorInput :: Char
  }

data Config = Config
  { dirName :: FilePath,
    lowercase :: Bool,
    uppercase :: Bool,
    recursive :: Bool,
    separator :: Separator
  }

readSeparatorInput :: Char -> Separator
readSeparatorInput c = case c of
  ' ' -> Space
  '-' -> Dash
  '_' -> Underscore
  _ -> error "Cannot read separator"

convertInput :: Input -> Config
convertInput input =
  Config
    { dirName = nameInput input,
      lowercase = lowercaseInput input,
      uppercase = uppercaseInput input,
      separator = readSeparatorInput $ separatorInput input,
      recursive = recursiveInput input
    }

replace :: (Eq a) => a -> a -> a -> a
replace c r t
  | t == c = r
  | otherwise = t

replaceMultiple :: (Eq a) => [a] -> a -> a -> a
replaceMultiple cs r x
  | x `elem` cs = r
  | otherwise = x

replaceMap :: (Eq a) => a -> a -> [a] -> [a]
replaceMap _ _ [] = []
replaceMap c r xs = fmap (replace c r) xs

config :: Parser Config
config =
  Config
    <$> argument
      str
      ( metavar "TARGET"
          <> help "Target directory to format"
      )
    <*> switch (long "lowercase" <> short 'l' <> help "Toggle to make everything lowercase")
    <*> switch (long "uppercase" <> short 'u' <> help "Toggle to make everything uppercase")
    <*> switch (long "recursive" <> short 'r' <> help "Toggle to format all the directory recursively")
    <*> option
      auto
      ( long "separator"
          <> short 's'
          <> metavar "SEPARATOR"
          <> help "Set the word separator"
          <> showDefault
          <> value Space
      )

mkFormatter :: Config -> FilePath -> FilePath
mkFormatter c = fmap (convertLower . convertUpper . convertSeparator)
  where
    convertLower = if lowercase c then toLower else id
    convertUpper = if uppercase c then toUpper else id
    convertSeparator = case separator c of
      Space -> replaceMultiple ['-', '_'] ' '
      Dash -> replaceMultiple [' ', '_'] '-'
      Underscore -> replaceMultiple [' ', '-'] '_'

formatFile :: Formatter -> FilePath -> IO ()
formatFile formatter filename
  | formatted == filename = return ()
  | otherwise = do
      isDir <- doesDirectoryExist filename
      if isDir
        then return ()
        else do
          putStrLn $ "Renaming " ++ filename ++ " to " ++ formatted
          rename filename formatted
  where
    formatted = formatter filename

formatFileOrRecurse :: Formatter -> FilePath -> IO ()
formatFileOrRecurse f file = do
  isDir <- doesDirectoryExist file
  if isDir
    then do
      contents <- listDirectory file
      withCurrentDirectory file $ mapM_ (formatFileOrRecurse f) contents
    else formatFile f file

hdir :: Config -> IO ()
hdir config = do
  let formatter = mkFormatter config
  contents <- listDirectory $ dirName config
  setCurrentDirectory $ dirName config
  let f = if recursive config then formatFileOrRecurse else formatFile
  mapM_ (f formatter) contents
  putStrLn ("Formatted all files in " ++ dirName config)

main :: IO ()
main = hdir =<< execParser opts
  where
    opts =
      info
        (config <**> helper)
        ( progDesc "Formats files in a directory"
            <> header "hdir - a directory formatter"
        )
