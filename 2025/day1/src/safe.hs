main :: IO ()
main = interact (show . (enterCombination (50, 0)) . words)

-- The state of the application of a combination. The first is the number to
-- which the dial is pointed. The second is the accumulation of the number of
-- times a zero is seen.
type CombinationState = (Int, Int)

enterCombination :: CombinationState -> [String] -> CombinationState
enterCombination start x = foldl rotate' start x
    where rotate' (position, zeros) move = toStateWithZerosCheck zeros $ rotate position move
          toStateWithZerosCheck currentZeros nextPosition = (nextPosition, currentZeros + if nextPosition == 0 then 1 else 0)

rotate :: Int -> String -> Int
rotate currentPosition ('L':cs) = mod (currentPosition - n) 100
    where n = read cs
rotate currentPosition ('R':cs) = mod (currentPosition + n) 100
    where n = read cs
