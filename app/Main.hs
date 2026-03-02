module Main where

import Naming
import Options.Applicative
import System.Directory (doesDirectoryExist, listDirectory, setCurrentDirectory, withCurrentDirectory)
import System.Posix.Files (rename)

type Formatter = FilePath -> FilePath

data Config = Config
  { dirName :: FilePath,
    recursive :: Bool,
    fstyle :: Style
  }

config :: Parser Config
config =
  Config
    <$> argument
      str
      ( metavar "TARGET"
          <> help "Target directory to format"
      )
    <*> switch (long "recursive" <> short 'r' <> help "Toggle to format the directory recursively")
    <*> option
      auto
      ( long "style"
          <> short 's'
          <> metavar "STYLE"
          <> help "Set the style for display. Options: Lower, Upper, Words, Snake, Camel, Kebab, Train"
      )

isHidden :: FilePath -> Bool
isHidden f = case f of
  [] -> False
  (x : _) -> x == '.'

listVisibleDirectory :: FilePath -> IO [FilePath]
listVisibleDirectory filename = do
  contents <- listDirectory filename
  return $ filter (not . isHidden) contents

mkFormatter :: Config -> Formatter
mkFormatter c = runFormat (fstyle c)

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
      contents <- listVisibleDirectory file
      withCurrentDirectory file $ mapM_ (formatFileOrRecurse f) contents
    else formatFile f file

hdir :: Config -> IO ()
hdir c = do
  let formatter = mkFormatter c
  contents <- listVisibleDirectory $ dirName c
  setCurrentDirectory $ dirName c
  let f = if recursive c then formatFileOrRecurse else formatFile
  mapM_ (f formatter) contents
  putStrLn ("Formatted all files in " ++ dirName c)

main :: IO ()
main = hdir =<< execParser opts
  where
    opts =
      info
        (config <**> helper)
        ( progDesc "Formats files in a directory"
            <> header "hdir - a directory formatter"
        )
