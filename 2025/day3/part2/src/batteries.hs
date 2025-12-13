import Data.Char (ord)

main :: IO ()
main = interact (show . sum . map (joltage 12) . banks . words)

type Bank = Integer

banks :: [String] -> [Bank]
banks = map read

toDigit :: Char -> Int
toDigit c = ord c - ord '0'

joltage :: Int -> Bank -> Integer
joltage n bank = read $ concat $ map show $ joltage' n (map toDigit $ show bank)

-- takes a bank as a list of Int instead of Bank type
joltage' :: Int -> [Int] -> [Int]
joltage' n digits
  | n == 0 = []
  | length digits <= n = digits
  | otherwise = highest : (joltage' (n-1) digits')
    where negative_indices = map ((*) (-1)) [0..] -- negating the indices allows for getting the left most max on matchin
          consideration = zip (take ((length digits) - (n - 1)) digits) negative_indices
          (highest, negative_index) = maximum consideration
          digits' = drop (-1*negative_index + 1) digits
