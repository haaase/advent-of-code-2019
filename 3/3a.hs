import           Data.List       (sortOn)
import           Data.List.Split
import           Data.Set        (Set)
import qualified Data.Set        as Set

-- Day 3: Crossed Wires

type Point = (Int,Int)  -- points are coordinates on a plane
type Wire = Set Point -- a wire is a set of points

-- calculate all crossed points from a starting point for a given direction command (e.g. R34 for "34 steps to the right")
calcPoints :: Point -> String -> Set Point
calcPoints start (direction:distance) = Set.fromList (points direction)
    where
        d = read distance
        x = fst start
        y = snd start
        points 'R' = [(xn,(snd start)) | xn <- [x .. (x+d)]]
        points 'L' = [(xn,(snd start)) | xn <- [(x - d) .. x]]
        points 'U' = [((fst start), yn) | yn <- [y .. (y+d)]]
        points 'D' = [((fst start), yn) | yn <- [y - d .. y]]

-- calculate the destination given a starting point and a direction command (e.g. R34)
getDestination :: Point -> String -> Point
getDestination start ('R':distance) = ((fst start + read distance),(snd start))
getDestination start ('L':distance) = ((fst start - read distance),(snd start))
getDestination start ('U':distance) = ((fst start), (snd start + read distance))
getDestination start ('D':distance) = ((fst start), (snd start - read distance))

-- given a starting point turn a list of movements into a wire (a set of crossed points)
parseWire :: Point -> [String] -> Wire
parseWire start (x:xs) = Set.union (calcPoints start x) (parseWire dest xs)
    where
        dest = getDestination start x
parseWire start [] = Set.empty

-- calculate taxicab distance of two points
distance :: Point -> Point -> Int
distance (p1,p2) (q1,q2) = (abs (p1 - q1)) + (abs (p2 - q2))

main = do
     input <- readFile "input.txt"
     let wires = map ((parseWire (0,0)). splitOn ",") (lines input) :: [Wire]

     let interSections = (wires !! 0) `Set.intersection` (wires !! 1) :: Set Point

     let distances = Set.map (\x -> (x, (distance (0,0) x))) interSections :: Set (Point, Int)
     let distancesSorted = filter (\x -> snd x /= 0) (sortOn snd (Set.toList distances))

     let solution = snd $ head distancesSorted

     putStrLn $ show solution
