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

dig = [1..9]
lowerLimit = 197487
upperLimit = 673251

toNum :: [Int] -> Int
toNum [a,b,c,d,e,f] = read $ (show a) ++ (show b) ++ (show c) ++ (show d) ++ (show e) ++ (show f)

possiblePasswords = [[a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] | a <- dig, b <-dig, c <- dig, d <- dig, e <- dig, f <- dig,
    a <= b, b <= c, c <= d, d <= e, e <= f,
    (a == b || b == c || c == d || d == e || e == f),
    toNum [a,b,c,d,e,f] >= lowerLimit, toNum [a,b,c,d,e,f] <= upperLimit]

main = do
    let solution = length possiblePasswords
    putStrLn $ show solution