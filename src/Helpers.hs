module Helpers where

import           Data.Char (isLower, toLower)


camelToSnakeCase :: String -> String
camelToSnakeCase [] = []
camelToSnakeCase (x:xs)
  | null t = h
  | otherwise = h <> "_" <> camelToSnakeCase t
  where
    (h, t) = span isLower (toLower x : xs)


-- | Simple pure functional queue.
-- Mostly used in tests.
data Queue a = Queue [a] [a]

emptyQueue :: Queue a
emptyQueue = Queue [] []

pushToQueue :: a -> Queue a -> Queue a
pushToQueue e (Queue xs ys) = Queue (e:xs) ys

pullFromQueue :: Queue a -> Maybe (a, Queue a)
pullFromQueue (Queue [] []) = Nothing
pullFromQueue (Queue xs []) = Just (y, Queue [] ys)
  where
    (y:ys) = reverse xs
pullFromQueue (Queue xs (y:ys)) = Just (y, Queue xs ys)

queueSize :: Queue a -> Int
queueSize (Queue xs ys) = length xs + length ys

queueToList :: Queue a -> [a]
queueToList (Queue xs ys) = ys <> reverse xs

listToQueue :: [a] -> Queue a
listToQueue xs = Queue [] (reverse xs)
