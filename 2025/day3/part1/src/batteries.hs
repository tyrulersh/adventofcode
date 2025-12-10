import Data.Char (ord)

main :: IO ()
main = interact (show . sum . map (joltage 2) . banks . words)

type Bank = Integer

banks :: [String] -> [Bank]
banks = map read

joltage :: Int -> Bank -> Integer
joltage n bank = last $ last $ take n rowsOfDigits
  where rowsOfDigits = firstRow:(zipWith3 attempting [2..] rowsOfDigits (repeat reverseDigits))
        reverseDigits = map readDigit $ reverse $ show bank
        firstRow = (reverseDigits !! 0):(zipWith max firstRow (tail reverseDigits))
        readDigit c = fromIntegral $ ord c - ord '0'

attempting :: Int -> [Integer] -> [Integer] -> [Integer]
attempting i previous reverseDigits = take (i - (length maxes)) negs ++ maxes
  where maxes = (head newPossibleMaxes):(zipWith max' maxes (tail newPossibleMaxes))
        negs = repeat $ fromIntegral (-1)
        newPossibleMaxes = zipWith (\p d -> 10*p + d) (drop (i-2) previous) (drop (i-1) reverseDigits)
        max' n m = max (reverseInteger n) (reverseInteger m)
        reverseInteger n = read $ reverse $ show n
