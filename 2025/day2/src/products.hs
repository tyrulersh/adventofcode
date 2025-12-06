import Data.Char (isSpace)

main :: IO ()
--main = interact (show . map invalid . concat . map readRange . splitOn ',')
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
invalid n = even numDigits && firstHalf == secondHalf
  where sn = show n
        numDigits = length sn
        firstHalf = take (quot numDigits 2) sn
        secondHalf = drop (quot numDigits 2) sn
