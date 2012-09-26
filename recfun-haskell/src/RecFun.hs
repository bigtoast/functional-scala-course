module RecFun where

-- Exercise 1 .. Pascals Triangle
pascal :: Int -> Int -> Int

pascal c r =
    if ( c == 0 || c == r )
    then
        1
    else
        pascal c (r - 1) + pascal ( c - 1 ) (r - 1 )


-- Exercise 2 .. Balance parens in a string
balance :: String -> Bool

balance str = step 0 str
    where
    step :: Int -> String -> Bool
    check :: Char -> Int
    check '('    = 1
    check ')'    = -1
    check a      = 0
    step acc str =
        if ( str == "" || acc < 0 )
        then
            acc == 0
        else
        step (acc + (check $ head str)) ( tail str )


-- Factor change
countChange :: Int -> [Int] -> Int
countChange 0 coins = 1
countChange money coins =
    if ( money < 0 || null coins )
    then
        0
    else
        (countChange money (tail coins)) + (countChange ( money - ( head coins ) ) coins)

