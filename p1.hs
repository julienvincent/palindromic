#!/usr/bin/env stack
-- stack --install-ghc runghc

{-
    let dist(i) be the value of the distance to the next palindrome from i

    Target 1)   Prove that sum dist(i) of all integers > 0 and < 1000 000 equates to 495076635 (done)
    Target 2)   Determine the sum of dist(i) of all integers > 0 and < 1000 000 000 000 (240H worth of calc time :\ ie: not done)
-}

_sum :: Int -> Int
_sum x = (x * x + x) `div` 2

-- Check if an int is a palindrome
check :: Int -> Bool
check i = do
    let s = show i :: String
    s == reverse s

-- Determine distance to next palindrome from i
dist :: Int -> Int -> Int
dist i acc = do
    let _i = i + acc
    if check _i
        then acc
        else dist i (acc + 1)

calc :: Int -> Int -> Int -> Int
calc start end acc = do
    if start >= end
        -- We are done. return the sum
        then acc
        else do
            -- Get the distance to the next palindome
            let distance = dist start 0
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