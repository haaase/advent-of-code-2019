import           Data.Char
import           Data.List.Split
import           Data.Sequence   (Seq, fromList, index, update)
import qualified Data.Text       as T

-- Day 5h: Sunny with a Chance of Asteroids

-- takes a string and a length and returns a zero-padded string of the requested length
pad0 :: String -> Int -> String
pad0 l x | (length l) >= x = l
         | otherwise = pad0 ('0':l) x

-- instruction parsing

-- takes an instruction and returns the opcode
op :: Int -> Int
op n = takeLast2 n where
    takeLast2 = read . reverse . (take 2) . reverse . show

modes :: Int -> (Int,Int,Int)
modes n = (a,b,c) where
    dig = (pad0 (show n) 5)
    a = digitToInt $ dig !! 0 :: Int
    b = digitToInt $ dig !! 1 :: Int
    c = digitToInt $ dig !! 2 :: Int

mode1 :: Int -> Int
mode1 n = third (modes n) where
    third (_,_,c) = c

mode2 :: Int -> Int
mode2 n = second (modes n) where
    second (_,b,_) = b

mode3 :: Int -> Int
mode3 n = first (modes n) where
    first (a,_,_) = a

-- interprets an intcode program, takes the program, the input and a program counter and returns the resulting program and the output
interp :: Seq Int -> Int -> [String] -> Int -> (Seq Int, [String])
interp prog input output pc
    | opcd == 1 = interp(update arg3 (val1 + val2) prog) input output (pc + 4)
    | opcd == 2 = interp(update arg3 (val1 * val2) prog) input output (pc + 4)
    | opcd == 3 = interp(update arg1 (input) prog) input output (pc + 2)
    | opcd == 4 = interp prog input (output ++ [show val1]) (pc + 2)
    | otherwise = (prog, output) -- finish execution and return program
    where instr = index prog pc
          opcd = op instr :: Int
          arg1 = index prog (pc+1) :: Int
          arg2 = index prog (pc+2) :: Int
          arg3 = index prog (pc+3) :: Int
          val1 | mode1 instr == 0 = index prog arg1 :: Int -- pos. mode
               | otherwise = arg1     -- imm. mode
          val2 | mode2 instr == 0 = index prog arg2 :: Int
               | otherwise = arg2

main = do
    input <- readFile "input.txt"
    let program = fromList (map read (splitOn "," (T.unpack $ T.strip $ T.pack input))) :: Seq Int

    let progIn = 1
    let progOut = snd (interp program progIn [] 0)

    let solution = unlines progOut

    putStrLn $ solution
