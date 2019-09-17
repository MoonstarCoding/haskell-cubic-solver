import Test.QuickCheck
import qualified Assign_1 as A1
-- import qualified Assign_1_ExtraCredit as A1E

main :: IO ()
main = do
    putStrLn "Test Suite Initialized:"
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
