import Data.List (nub, sort)
import Data.List.Split (splitOn)

main :: IO ()
main = interact (selector . words)
  where selector ("part1":ws) = part1 ws
        selector ("part2":ws) = part2 ws
        part1 = show . length . spoiles . parse
        part2 = show . freshies . parse

type Fresh = [(Integer,Integer)]
type Ingredients = [Integer]
type DB = (Fresh, Ingredients)

parse :: [String] -> DB
parse = foldl combine ([],[]) . map singleton
  where singleton s
          | xs == [] = ([],[read x])
          | otherwise = ([(read x,read (head xs))],[])
          where x:xs = splitOn "-" s

combine :: DB -> DB -> DB
combine (a,b) (x,y) = (a++x,b++y)

freshies :: DB -> Integer
freshies = freshies' . fst
  where freshies' = sum . map rangeSize . contiguate . sort

contiguate :: Fresh -> Fresh
contiguate l = foldr reducer [head l] $ reverse l
  where reducer next accum = mergeIf next (head accum) ++ tail accum
        mergeIf t u = if t `intersects` u then [merge t u] else [t,u]
        merge (a,b) (x,y) = (min a x, max b y)

intersects :: (Integer,Integer) -> (Integer,Integer) -> Bool
intersects v@(a,b) w@(x,y) = x `inn` v || y `inn` v || a `inn` w || b `inn` w

inn :: Integer -> (Integer,Integer) -> Bool
inn x (a,b) = x >= a && x <= b

rangeSize :: (Integer,Integer) -> Integer
rangeSize (x,y) = y - x + 1

spoiled :: Fresh -> Integer -> Bool
spoiled ranges i = dropWhile (outOfRange i) ranges /= []
  where outOfRange i (low,high) = i < low || i > high

spoiles :: DB -> [Integer]
spoiles (_, []) = []
spoiles (ranges, i:is)
  | spoiled ranges i = i:spoiles'
  | otherwise = spoiles'
    where spoiles' = spoiles (ranges, is)
