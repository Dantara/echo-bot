module Helpers where

import           Data.Char


camelToSnakeCase :: String -> String
camelToSnakeCase [] = []
camelToSnakeCase (x:xs)
  | null t = h
  | otherwise = h <> "_" <> camelToSnakeCase t
  where
    (h, t) = span isLower (toLower x : xs)
