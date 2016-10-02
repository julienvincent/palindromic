#!/usr/bin/env stack
-- stack --install-ghc runghc

{-
    let dist(i) be the value of the distance to the next palindrome from i

    Target 1)   Prove that sum dist(i) of all integers > 0 and < 1000 000 equates to 495076635 (done)
    Target 2)   Determine the sum of dist(i) of all integers > 0 and < 1000 000 000 000 (done. ~27s calc time)
-}

_sum :: Int -> Int
_sum x = (x * x + x) `div` 2

-- Check if an int is a palindrome
check :: Int -> Bool
check i =
    let s = show i
    in  s == reverse s

roundUp :: Int -> Int -> Int
roundUp num numLength =
    let increment = 10 ^ (numLength `div` 2)
    in  (ceiling ((fromIntegral num ::Double) / (fromIntegral increment))) * increment

findNext :: Int -> Int
findNext i =
    let num = show i
        numLength = length num
        middleIndex = (numLength - 1) `div` 2

        half = take (numLength `div` 2) num
        middle = num!!middleIndex

        multiplier = 10 ^ (numLength `div` 2)
        oddMultiplier = floor (1.1 * (fromIntegral multiplier) :: Double)

        (increment, newNum) =
            if even numLength
                then (oddMultiplier, read (half ++ (reverse half)))
                else (multiplier, read (half ++ [middle] ++ (reverse half)))
    in
        if newNum > i
            then newNum
            else if middle == '9'
                then findNext (roundUp newNum numLength)
                else newNum + increment

-- Determine distance to next palindrome from i
dist :: Int -> Int
dist i
   | check i    = 0
   | otherwise  = (findNext i) - i

calc :: Int -> Int -> Int -> Int
calc start end acc = do
    if start >= end
        -- We are done. return the sum
        then acc
        else do
            -- Get the distance to the next palindrome
            let distance = dist start
            let nextIteration = start + distance

            -- Distance will be 0 for single digit palindromes. Skip them.
            if distance == 0
                then calc (start + 1) end acc
                else if nextIteration > end
                    -- Add all iterations until the end of the sequence
                    then do
                        let diff = nextIteration - end
                        let totalSum = _sum distance
                        let diffSum = _sum diff
                        let nextAcc = totalSum - diffSum
                        calc (start + nextIteration) end (acc + nextAcc)
                    -- Add all iterations until the next palindrome
                    else calc (nextIteration + 1) end (acc + (_sum distance))

main :: IO()
main = do
   print ("Iterations?:")
   runXTimes <- getLine
   print ("result: " ++ show (calc 0 (read runXTimes :: Int) 0) :: String)