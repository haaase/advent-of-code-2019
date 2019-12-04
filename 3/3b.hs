import           Data.List       (elemIndex, sortOn)
import           Data.List.Split
import           Data.Set        (Set)
import qualified Data.Set        as Set

-- Day 3: Crossed Wires

type Point = (Int,Int)  -- points are coordinates on a plane
type Wire = [Point] -- a wire is a list of points

-- calculate all crossed points from a starting point for a given direction command (e.g. R34 for "34 steps to the right")
calcPoints :: Point -> String -> [Point]
calcPoints start (direction:distance) = points direction
    where
        d = read distance
        x = fst start
        y = snd start
        points 'R' = [(xn,(snd start)) | xn <- [x+1 .. (x+d)]]
        points 'L' = reverse [(xn,(snd start)) | xn <- [(x - d) .. x-1]]
        points 'U' = [((fst start), yn) | yn <- [y+1 .. (y+d)]]
        points 'D' = reverse [((fst start), yn) | yn <- [y - d .. y-1]]

-- calculate the destination given a starting point and a direction command (e.g. R34)
getDestination :: Point -> String -> Point
getDestination start ('R':distance) = ((fst start + read distance),(snd start))
getDestination start ('L':distance) = ((fst start - read distance),(snd start))
getDestination start ('U':distance) = ((fst start), (snd start + read distance))
getDestination start ('D':distance) = ((fst start), (snd start - read distance))

-- given a starting point turn a list of movements into a wire (a set of crossed points)
parseWire :: Point -> [String] -> Wire
parseWire start (x:xs) = (calcPoints start x) ++ (parseWire dest xs)
    where
        dest = getDestination start x
parseWire start [] = []

-- calculate taxicab distance of two points
distance :: Point -> Point -> Int
distance (p1,p2) (q1,q2) = (abs (p1 - q1)) + (abs (p2 - q2))

-- calculate how many steps it takes for a wire to reach a certain point, returns -1 if the point is not part of the wire
steps :: Wire -> Point -> Int
steps wire point = case (elemIndex point wire) of
    Just (i) -> i + 1
    Nothing  -> -1

main = do
     input <- readFile "input.txt"
     let wires = map ((parseWire (0,0)). splitOn ",") (lines input) :: [Wire]
     let w1 = (wires !! 0)
     let w2 = (wires !! 1)

     let interSections = Set.fromList w1 `Set.intersection` (Set.fromList w2) :: Set Point

     let stepsToIntersec = map (\x -> (x, (steps w1 x) + (steps w2 x))) (Set.toList interSections)

     let solution = snd $ head $ sortOn snd stepsToIntersec

     putStrLn $ show solution
