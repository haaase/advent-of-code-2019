{-# LANGUAGE DataKinds #-}
import  Data.Sequence
import           Data.List.Split
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

main = do
     input <- readFile "input.txt"
     let program = fromList (map read (splitOn "," (T.unpack $ T.strip $ T.pack input))) :: Seq Int
     let program1202 = update 2 2 (update 1 12 program) -- restore 1202 error state
     putStrLn $ show (interp program1202 0)
