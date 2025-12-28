import Data.List (nub, sort, elemIndices, elemIndex)

main :: IO ()
main = interact (selector . lines)
  where selector ("part1":ws) = show $ part1 ws
        selector ("part2":ws) = show $ part2 ws

part1 :: [String] -> Int
part1 (l:ls) = walk ls (map sToPipe l) 0

sToPipe :: Char -> Char
sToPipe 'S' = '|'
sToPipe c = c

walk :: [String] -> String -> Int -> Int
walk [] _ accum = accum
walk (x:xs) previous accum = walk xs next (accum + splits)
  where (next, splits) = step previous x

-- steps one line forward, returning the update of current and the nubmer of splits:
-- step previous current = (next, numberOfSplits)
step :: String -> String -> (String,Int)
step previous current = (current', length splitIndices)
  where beamIndices = elemIndices '|' previous
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

part2 :: [String] -> Int
part2 (l:ls) = timelines ls i
  where (Just i) = elemIndex 'S' l

-- main solution to part2: figuring out the number of paths
timelines :: [String] -> Int -> Int
timelines [] _ = 1
timelines (x:xs) i
  | i >= length x = 0
  | x !! i == '^' = (timelines xs (i-1)) + (timelines xs (i+1))
  | otherwise = timelines xs i
