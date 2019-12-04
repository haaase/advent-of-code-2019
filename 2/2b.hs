{-# LANGUAGE DataKinds #-}
import  Data.Sequence(update,index, fromList, Seq)
import           Data.List.Split
import           Data.List(find)
import qualified Data.Text as T

-- Day 2: Intcode Interpreter

-- interprets an intcode program, takes the program and a program counter as input and returns the resulting program
interp :: Seq Int -> Int -> Seq Int
interp prog pc
    | opcd == 1 = interp(update arg3 (val1 + val2) prog) (pc + 4)
    | opcd == 2 = interp(update arg3 (val1 * val2) prog) (pc + 4) 
    | otherwise = prog -- finish execution and return program
    where opcd = index prog pc :: Int
          arg1 = index prog (pc+1) :: Int
          arg2 = index prog (pc+2) :: Int
          arg3 = index prog (pc+3) :: Int
          val1 = index prog arg1 :: Int
          val2 = index prog arg2 :: Int

-- helper functions
-- sets the two input parameters of a program (noun and verb)
setInput :: Seq Int -> (Int, Int) -> Seq Int
setInput program (a,b) = update 2 b (update 1 a program)

getOutput :: Seq Int -> Int
getOutput prog = index prog 0

run :: Seq Int -> Seq Int
run prog = interp prog 0

main = do
     input <- readFile "input.txt"
     let program = fromList (map read (splitOn "," (T.unpack $ T.strip $ T.pack input))) :: Seq Int
     
     let inputCombinations = [(noun,verb) | noun <- [1..99], verb <- [1..99]]
     let outputs = map (getOutput . run . (setInput program)) inputCombinations
     let inputsOutputs = zip inputCombinations outputs :: [((Int,Int), Int)]
     
     let solution = case (find (\x -> snd(x) == 19690720) inputsOutputs) of
                    Just((noun,verb), output) -> show (100*noun + verb)
                    Nothing -> "Error"
     
     putStrLn $ solution
