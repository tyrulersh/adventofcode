import Data.Char (isSpace)

main :: IO ()
main = interact (show . sum . filter invalid . concat . map readRange . map (filter (\x -> not $ isSpace x)) . splitOn ',')

splitOn :: Char -> String -> [String]
splitOn c list = l : remainder
  where (l, r) = break ((==) c) list
        remainder = case r of
          [] -> []
          _:xs -> splitOn c xs

readRange :: String -> [Int]
readRange range = [start .. end]
  where (startString, minusEnd) = break ((==) '-') range
        start = read startString
        end = read (tail minusEnd)

-- is the number invalid, which means its some other number whose digits are concatenated twice
invalid :: Int -> Bool
invalid n = or (map ((==) sn) repeats)
  where sn = show n
        numDigits = length sn
        -- don't include any chunk size that's the full length because that's the whole number and not repeated
        chunkSizes = filter ((/=) numDigits) $ factors numDigits
        -- units are the possible repeating components
        units = [take x sn | x <- chunkSizes]
        -- all possible repeat pattens tha may or may not be equal to n
        repeats = map (take numDigits . cycle) units

factors :: Int -> [Int]
factors n = lowDivisors ++ highDivisors
  where divides p x = mod p x == 0
        lowDivisors = filter (divides n) $ takeWhile (\x -> x*x <= n) [1 ..]
        -- lowDivisors will already have any possible square root divisor, so filter that one out here
        -- Also, reversing the lowDivisors means the resulting list here will be sorted.
        highDivisors = filter (\x -> x*x /= n) $ map (quot n) $ reverse lowDivisors
