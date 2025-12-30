import Data.List (nub, sortBy, (\\))
import Data.List.Split (splitOn)

main :: IO ()
main = interact (selector . lines)
  where selector ("part1":count:junctions) = show $ part1 (map readJunction junctions) (read count)
        selector ("part2":ws) = "123"

type Junction = (Int,Int,Int)
type Metric = Junction -> Junction -> Double
-- (distance, sourceIndex, destinationIndex)
type Edge = (Double,Int,Int)
-- list of indices in the circuit
type Circuit = [Int]

readJunction :: String -> Junction
readJunction = toTuple . map read . splitOn ","
  where toTuple [x,y,z] = (x,y,z)

part1 :: [Junction] -> Int -> Int
part1 junctions count = product $ map length $ take 3 $ sortBy largestCircuit circuits
  where edgesToConnect = take count $ sortBy smallestEdge $ edges euclidean junctions
        circuits = connect edgesToConnect

smallestEdge :: Edge -> Edge -> Ordering
smallestEdge (d,_,_) (d',_,_) = compare d d'

largestCircuit :: Circuit -> Circuit -> Ordering
largestCircuit x y = (flip compare) (length x) (length y)

connect :: [Edge] -> [Circuit]
connect es = connect' nodes
  where allNeighbors = neighbors es
        nodes = nub $ [p | (_,p,_) <- es] ++ [q | (_,_,q) <- es]
        connect' [] = []
        connect' (n:ns) = reachableFromN:(connect' ns')
          where reachableFromN = reachable n allNeighbors
                ns' = ns \\ reachableFromN

neighbors :: [Edge] -> [[Int]]
neighbors es = map findNeighbors [0..maxIndex]
  where maxIndex = maximum nodes
        nodes = nub $ concat $ [[p,q] | (_,p,q) <- es]
        findNeighbors i = nub [if q == i then p else q | (_,p,q) <- es, p == i || q == i]

-- reachable i ns = all nodes reachable from i according to the neighbors list, ns
reachable :: Int -> [[Int]] -> [Int]
reachable i allNeighbors = bfs [] [i]
  where bfs visited [] = visited
        bfs visited (q:qs)
          | q `elem` visited = bfs visited qs 
          | otherwise = bfs (q:visited) $ qs ++ (allNeighbors !! q)

-- from a list of junctions, returns list of all n^2 edges in form of (d,x,y) where d is the distance between junctions
-- x and y. And x and y are not the junctions, but rather the index into the input junction list of the junctions.
-- The distance is as per measured by the input metric.
-- Edges to self will not be in the list.
-- Edges are not directional, meaning only one of p->q or q->p will be in the list.
edges :: Metric -> [Junction] -> [Edge]
edges metric junctions = [(metric p q, i, j) | (p,i) <- junctionIndices, (q,j) <- (drop (i+1) junctionIndices)] -- using only values after i ensures no self edges and no duplicates for i,j and j,i
  where junctionIndices = zip junctions [0..]

euclidean :: Metric
euclidean (a,b,c) (x,y,z) = sqrt (fromIntegral (m1 + m2 + m3))
  where m1 = (a-x)^2
        m2 = (b-y)^2
        m3 = (c-z)^2
