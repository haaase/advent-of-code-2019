-- Day 4: Secure Container

-- However, they do remember a few key facts about the password:
--
--     It is a six-digit number.
--     The value is within the range given in your puzzle input.
--     Two adjacent digits are the same (like 22 in 122345).
--     Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
--
-- Other than the range rule, the following are true:
--
--     111111 meets these criteria (double 11, never decreases).
--     223450 does not meet these criteria (decreasing pair of digits 50).
--     123789 does not meet these criteria (no double).

import Data.List

lowerLimit = 197487
upperLimit = 673251

increasing :: Int -> Bool
increasing n = isInc s where
    s = show n
    isInc [] = True
    isInc (a:[]) = True
    isInc (a:xs@(b:_)) = (a <= b) && (isInc xs)
        
containsPair :: Int -> Bool
containsPair n = 2 `elem` subSequenceCounts where
    groups = group (show n)
    subSequenceCounts = map length groups

main = do
    let possiblePasswords = filter (\x -> (increasing x) && (containsPair x))[lowerLimit..upperLimit]
    let solution = length possiblePasswords
    putStrLn $ show solution