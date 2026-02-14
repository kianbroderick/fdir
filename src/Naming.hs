module Naming where

import Data.Char (toLower, toUpper)

type Name = [String]

data Style
  = Lower
  | Upper
  | Words
  | Snake
  | Camel
  | Kebab
  | Train
  deriving (Read)

runFormat :: Style -> String -> String
runFormat style s =
  let w = toWords s
   in case style of
        Lower -> fmap toLower s
        Upper -> fmap toUpper s
        Words -> concatWith " " w
        Snake -> toLower <$> concatWith "_" w
        Camel -> toCamel w
        Kebab -> toLower <$> concatWith "-" w
        Train -> concatWith "-" (fmap capitalize w)

toCamel :: [String] -> String
toCamel [] = []
toCamel (x : xs) = x ++ concatMap capitalize xs

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

concatWith :: String -> [String] -> String
concatWith _ [] = []
concatWith s (x : xs) = x ++ concatMap (s ++) xs

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : fmap toLower xs

toWords :: String -> [String]
toWords s = words $ fmap (replaceMultiple ['-', '_'] ' ') s
