#!/usr/bin/env stack
-- stack --install-ghc runghc

{-
    let dist(i) be the value of the distance to the next palindrome from i

    Target 1)   Prove that sum dist(i) of all integers > 0 and < 1000 000 equates to 495076635 (done)
    Target 2)   Determine the sum of dist(i) of all integers > 0 and < 1000 000 000 000 (done. ~27s calc time)
-}

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
                -- even reconstruction
                then (oddMultiplier, read (half ++ (reverse half)))
                -- odd reconstruction
                else (multiplier, read (half ++ [middle] ++ (reverse half)))

        roundUp :: Int -> Int -- round num up to next ^10 from center digit
        roundUp num =
            let increment = 10 ^ (numLength `div` 2)
            in (ceiling ((fromIntegral num :: Double) / (fromIntegral increment))) * increment

    in if newNum > i
        -- we have the next palindrome
        then newNum
        else if middle == '9'
            -- must round up the middle number of newNum and then try again
            then findNext (roundUp newNum)
            -- newNum + increment is the next palindrome
            else newNum + increment

dist :: Int -> Int -- Determine distance to next palindrome from i
dist i =
    let check :: Int -> Bool -- Check if an int is a palindrome
        check i =
            let s = show i
            in s == reverse s

    in if check i
        then 0
        else (findNext i) - i

calc :: Int -> Int -> Int -> Int -- calc sum of dist(i) over n
calc start end acc =
    let _sum :: Int -> Int -- nth triangular equation
        _sum x = (x * x + x) `div` 2

    in if start >= end
        -- We are done. return the sum
        then acc
        else
            -- Get the distance to the next palindrome
            let distance = dist start
                -- determine iterations to skip
                nextIteration = start + distance

            -- Distance will be 0 for single digit palindromes. Skip them.
            in if distance == 0
                then calc (start + 1) end acc
                else if nextIteration > end
                    -- Add all iterations until the end of the sequence
                    then
                        let diff = nextIteration - end
                            totalSum = _sum distance
                            diffSum = _sum diff
                            nextAcc = totalSum - diffSum
                        in calc (start + nextIteration) end (acc + nextAcc)
                    -- Add all iterations until the next palindrome
                    else calc (nextIteration + 1) end (acc + (_sum distance))

main :: IO()
main = do
   print ("Iterations?:")
   runXTimes <- getLine
   print ("result: " ++ show (calc 0 (read runXTimes :: Int) 0) :: String)