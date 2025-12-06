main :: IO ()
main = interact (show . (enterCombination (CombinationState 50 0 0) . words))

-- The state of the application of a combination. The first is the number to
-- which the dial is pointed. The second is the accumulation of the number of
-- times a zero is seen.
data CombinationState = CombinationState {
    position :: Int,
    zeros :: Int,
    allZeros :: Int
} deriving (Show)

enterCombination :: CombinationState -> [String] -> CombinationState
enterCombination start = foldl rotate' start

rotate' :: CombinationState -> String -> CombinationState
rotate' state move@(direction:xs) = CombinationState position' zeros' allZeros'
    where position' = rotate (position state) move
          zeros' = (zeros state) + if position' == 0 then 1 else 0
          allZeros' = (allZeros state) + (quot steps 100) + if position state /= 0 && wraps direction then 1 else 0
          wraps 'L' = position' > (position state) || position' == 0
          wraps 'R' = position' < (position state)
          steps = read xs

rotate :: Int -> String -> Int
rotate currentPosition ('L':cs) = mod (currentPosition - n) 100
    where n = read cs
rotate currentPosition ('R':cs) = mod (currentPosition + n) 100
    where n = read cs
