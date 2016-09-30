#!/usr/bin/env stack
-- stack --install-ghc runghc

check :: Int -> Bool
check i = do
    let s = show i :: String
    s == reverse s

dist :: Int -> Int -> Int
dist i acc = do
    let _i = i + acc
    if check _i
        then acc
        else dist i (acc + 1)

calc :: Int -> Int -> Int -> Int -> Int
calc start end acc cache = do
    if start == end
        then acc
        else do
            let distance = if cache < 0
                then dist start 0
                else cache
            calc (start + 1) end (acc + distance) (distance - 1)

main :: IO()
main = do
    print ("Iterations?:")
    runXTimes <- getLine
    print ("result: " ++ show (calc 0 (read runXTimes :: Int) 0 (-1)) :: String)