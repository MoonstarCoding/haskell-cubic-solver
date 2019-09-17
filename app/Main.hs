module Main where

import qualified Assign_1 as A1
-- import qualified Assign_1_ExtraCredit as A1E

main :: IO ()
main = do
    putStrLn "Executable for 1JC3-Assign1 successfully run:"

    putStrLn "Here are some example calculations and their results:"
    putStrLn "cubicRealSolutions 1 (-6) 11 (-6)"
    print(cubicRealSolutions 1 (-6) 11 (-6))
    putStrLn "cubicRealSolutions 1 0 (-3) 0"
    print(cubicRealSolutions 1 (-6) 11 (-6))
    putStrLn "cubicRealSolutions 1 (-3) 3 (-1)"
    print(cubicRealSolutions 1 (-6) 11 (-6))
    putStrLn "cubicRealSolutions 1 (-5) 8 (-4)"
    print(cubicRealSolutions 1 (-6) 11 (-6))

    putStrLn "Now it is your turn:"
    putStrLn "Choose an A Value:"
    va <- getLine
    putStrLn "Choose a B Value:"
    vb <- getLine
    putStrLn "Choose a C Value:"
    vc <- getLine
    putStrLn "Choose a D Value:"
    vd <- getLine
    let a = read va
        b = read vb
        c = read vc
        d = read vd
    putStrLn "The roots of your polynomial equation are: "
    print(cubicRealSolutions a b c d)