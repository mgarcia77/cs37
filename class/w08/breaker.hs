
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p [] = ([], [])
break' p lst@(x:xs) -- aliasing
  | p x = ([], lst)
-- | otherwise = let y = break' p xs
--                in (x:(fst y), (snd y))
| otherwise = (x:(fst y), (snd y))
              where y = break' p xs

{-
cycle' :: [a] -> [a]
cycle' [] = []
cycle' lst = lst ++ (cycle' lst)
-}

cycle' :: [a] -> [a]
cycle' [] = []
cycle' lst = helper lst
  where helper [] = cycle' lst
        helper (x:xs) = x : (helper xs)
