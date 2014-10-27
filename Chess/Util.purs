module Chess.Util where

takeWhile :: forall a. (a -> Boolean) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) =
  if (p x) then x : takeWhile p xs else []

dropWhile :: forall a. (a -> Boolean) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p lst@(x:xs) =
  if (p x) then dropWhile p xs else lst
