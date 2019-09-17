module Assign_1_ExtraCredit where

import Data.Complex
-- see https://www.stackage.org/haddock/lts-8.24/base-4.9.1.0/Data-Complex.html

macid :: String
macid = "hutchm6"

{-
    - Assignment 1
    - Name: Mark Hutchison
    - Date: September 17th, 2019
 -}

{-
    Similar to the normal assignment, I will be using the 4 base equations that will be listed under this comment string. That being said, because the equations for Q R and Disc do not change, I will not be re showing the outputs for those above the functions.

    - cubicRealSolutions 1 (-6) 11 (-6) = should [1,2,3]
    - cubicRealSolutions 1 0 (-3) 0 = [0, sqrt 3, -sqrt 3]
    - cubicRealSolutions 1 (-3) 3 (-1) = should [1,1,1]
    - cubicRealSolutions 1 (-5) 8 (-4) = should [1,2,2]
-}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3 * a * c) - (b ** 2)) / (9 * (a ** 2))

cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9 * a * b * c) - (27 * (a ** 2) * d) - (2 * (b ** 3))) / (54 * (a ** 3))

cubicDisc :: Double -> Double -> Double
cubicDisc q r = do
    if r == 0
        then q
        else ((q ** 3) + (r ** 2)) ** (1 / 3)

{-
    Here is the point where everything changes and we need to begin thinking in complex number systems.

    We need to be able to get the square root of a negative number, so the S and T function needed to change.
-}

cubicComplexS :: Double -> Double -> Complex Double
cubicComplexS q r = s where
    s = cubeRoot (r + sqrt (cubicDisc q r))

{-

-}

cubicComplexT :: Double -> Double -> Complex Double
cubicComplexT q r = t where
    t = cubeRoot (r - sqrt (cubicDisc q r))

cubicComplexSolutions :: Double -> Double -> Double -> Double -> [Complex Double]
cubicComplexSolutions a b c d = [x1, x2, x3] where
    q = cubicQ a b c
    r = cubicR a b c d
    s = cubicComplexS q r
    t = cubicComplexT q r
    x1 = (s + t - (b/(3*a)))
    x2 = ((-1) * ((s + t) / 2) - (b/(3*a)) + (((sqrt 3)/(2)) * (s - t)))
    x3 = ((-1) * ((s + t) / 2) - (b/(3*a)) - (((sqrt 3)/(2)) * (s - t)))

cubeRoot :: Double -> Double
cubeRoot n = do
    if n == 0
        then 0
        else
            if n > 0
                then n ** (1/3)
                else
                    if n < 0
                        then (-1) * ((-n) ** (1/3))
                        else n ** (1/3)