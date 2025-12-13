import Data.Char (ord)
import Data.List (intercalate)

main :: IO ()
main = interact (show . selector . words)
  where selector ("part1":ws) = part1 ws
        selector ("part2":ws) = part2 ws
        part1 = sumGrid . accessibleRolls . parser
        part2 = sum . takeWhile (/= 0) . map sumGrid . unloads . parser

type ExternalGrid = [String]
type Grid = [[Int]]
-- neighborhood of any size
type NeighborHood = Grid
type Rule = NeighborHood -> Int
type Parser = ExternalGrid -> Grid
type Reader = Grid -> [String]

-- gets the sequence of unloaded accessible rolls, infinite list
unloads :: Grid -> [Grid]
unloads grid = zipWith unloadedItems unloadResults (tail unloadResults)
  where unloadResults = iterate unloadAccessible grid
        -- Determines which items were unloaded by subtracting those that remain from those that were there
        unloadedItems = zipWithGrid (-)

unloadAccessible :: Grid -> Grid
unloadAccessible grid = zipWithGrid (-) grid $ accessibleRolls grid

parser :: Parser
parser = mapGrid parser'
  where parser' '@' = 1
        parser' '.' = 0

accessibleRolls :: Grid -> Grid
accessibleRolls grid = zipWithGrid (*) grid $ mapGrid fewerThanFourRule neighborhoodsGrid
  where neighborhoodsGrid = mapGrid (neighbors grid) indexGrid
        indexGrid = zipWith indexer grid [0..]
        indexer row rowIndex = take (length row) $ zip (repeat rowIndex) [0..]

sumGrid :: Grid -> Int
sumGrid = sum . concat

zipWithGrid :: (Int -> Int -> Int) -> Grid -> Grid -> Grid
zipWithGrid f = zipWith (zipWith f)

mapGrid :: (a -> b) -> [[a]] -> [[b]]
mapGrid f = map (map f)

fewerThanFourRule :: Rule
fewerThanFourRule hood = fromEnum $ strictNeighborsCount < 4
  where strictNeighborsCount = sum $ zipWith (*) [1,1,1,1,0,1,1,1,1] (concat hood)

neighbors :: Grid -> (Int, Int) -> NeighborHood
neighbors grid coord = mapGrid (gridValue grid) $ mapGrid (tupleAdd coord) offsetsGrid
  where offsetsGrid = [zip (take 3 $ repeat x) y | x <- [-1,0,1], y <- [[-1,0,1]]]
        tupleAdd (a,b) (a',b') = (a+a', b+b')

gridValue :: Grid -> (Int, Int) -> Int
gridValue grid (i,j) = if outOfBounds then 0 else (grid !! i) !! j
  where h = length grid
        w = length $ head grid
        outOfBounds = i < 0 || i > h-1 || j < 0 || j > w-1
