main :: IO ()
main = interact (show . (enterCombination (CombinationState 50 0) . words))

-- The state of the application of a combination. The first is the number to
-- which the dial is pointed. The second is the accumulation of the number of
-- times a zero is seen.
data CombinationState = CombinationState {
    position :: Int,
    zeros :: Int
} deriving (Show)

enterCombination :: CombinationState -> [String] -> CombinationState
enterCombination start = foldl rotate' start
    where rotate' state move = toStateWithZerosCheck (zeros state) $ rotate (position state) move
          toStateWithZerosCheck currentZeros nextPosition = CombinationState nextPosition (currentZeros + if nextPosition == 0 then 1 else 0)

rotate :: Int -> String -> Int
rotate currentPosition ('L':cs) = mod (currentPosition - n) 100
    where n = read cs
rotate currentPosition ('R':cs) = mod (currentPosition + n) 100
    where n = read cs
