import Data.List (nub, sort)

main :: IO ()
main = interact (selector . lines)
  where selector ("part1":ws) = show $ part1 ws
        selector ("part2":ws) = "123"

part1 :: [String] -> Int
part1 (l:ls) = walk ls l 0

walk :: [String] -> String -> Int -> Int
walk [] _ accum = accum
walk (x:xs) previous accum = walk xs next (accum + splits)
  where (next, splits) = step previous x

-- steps one line forward, returning the update of current and the nubmer of splits:
-- step previous current = (next, numberOfSplits)
step :: String -> String -> (String,Int)
step previous current = (current', length splitIndices)
  where beamIndices = [i | (x,i) <- (zip previous [0..]), x == '|' || x == 'S']
        splitIndices = [i | i <- beamIndices, current !! i == '^']
        continuences = [i | i <- beamIndices, current !! i == '.']
        -- indices of all beams in new row resulting from a split
        splits = filter inBounds $ concat $ map (\i -> [i-1, i+1]) splitIndices
        inBounds i = i >= 0 && i < length current
        beamIndices' = sort $ nub $ continuences ++ splits
        current' = overlay (zip current [0..]) beamIndices'
        overlay x [] = map fst x
        overlay ((c, i):cs) beams@(x:xs)
          | i < x = c:(overlay cs beams)
          | i == x = '|':(overlay cs xs)
          -- otherwise isn't possible, let it fail if it happens
