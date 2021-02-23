module Data.List.Extra
  ( mapWithIndex
  , mapIf
  )
where

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0 ..]

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf cond f = map (\x -> if cond x then f x else x)
