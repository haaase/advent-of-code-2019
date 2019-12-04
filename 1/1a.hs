calcFuel :: (Integral n) => n -> n
calcFuel n = floor (fromIntegral n / 3) - 2

main = do
     input <- readFile "input.txt"
     let weights = map read (lines s) :: [Int]
     putStrLn $ show (sum (map calcFuel weights))