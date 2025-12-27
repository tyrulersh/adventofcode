import Data.List (nub, sort)
import Data.List.Split (splitOn)

main :: IO ()
main = interact (selector . lines)
  where selector ("part1":ws) = part1 ws
        selector ("part2":ws) = part2 ws
        part2 _ = show 123

part1 ws = show $ sum $ zipWith sumProductSelector operators $ foldTuples $ map toTuples $ map readNumberLine $ filter (not . isOperatorLine) ws
  where foldTuples ts = foldl (zipWith sumProduct) (replicate (length operators) (0,1)) ts
        operators = words $ last ws
        isOperatorLine line = '*' `elem` line || '+' `elem` line
        sumProductSelector "+" = fst
        sumProductSelector "*" = snd

readNumberLine :: String -> [Integer]
readNumberLine = map read . words

toTuples :: [Integer] -> [(Integer,Integer)]
toTuples = map (\x -> (x,x))

sumProduct :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
sumProduct (a,b) (x,y) = (a+x, b*y)
