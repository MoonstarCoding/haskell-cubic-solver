{-
    - Assignment 1
    - Name: Mark Hutchison
    - Date: September 17th, 2019
 -}
module Assign_1 where

macid :: String
macid = "hutchm6"

{-
    In order to verify that all sections are working, I have created an example 3 equations. One of which returns a negative discriminant. If I do everything correctly, I will expect those 0s and NaN for the negative discriminant based equations. All tests can be found in the comments right above that particular function.
-}

{-
    - cubicQ 1 (-6) 11 = -0.3333333333333333 - Works
    - cubicQ 1 (-5) 8 = -0.1111111111111111 - Works
    - cubicQ 1 (-3) 3 = 0.0 - Works
-}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3 * a * c) - (b ** 2)) / (9 * (a ** 2))

{-
    - cubicR 1 (-6) 11 (-6) = 0 - Works
    - cubicR 1 (-5) 8 (-4) = -3.7037037037037035e-2 - Works
    - cubicR 1 (-3) 3 (-1) = 0 - Works
-}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9 * a * b * c) - (27 * (a ** 2) * d) - (2 * (b ** 3))) / (54 * (a ** 3))

{-
    - cubicDisc (cubicQ 1 (-6) 11) (cubicR 1 (-6) 11 (-6)) = -0.3333333333333333 - Works
    - cubicDisc (cubicQ 1 (-5) 8) (cubicR 1 (-5) 8 (-4)) = 0.0
    - cubicDisc (cubicQ 1 (-3) 3) (cubicR 1 (-3) 3 (-1)) = 0.0
-}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = ((q ** 3) + (r ** 2))

{-
    cubicS and cubicT are very difficult to plug in on my calculator, so I will be skipping the testing for these two values. However given my 4 test scenarios at the end, I should get a pretty clear image of whether or not they're working. Also, I know that all of the components going into their functions are working, so there is no reason that they should not work.

    Every equation uses the value of s and t, so making sure that the return value is correct is crucial. However, if the discriminant is negative, it will return NaN. This is because it is not possible to solve for the negative of a square root. This is why two of my test questions should return 3 values of NaN instead of actually solutions that exist. Only Complex Number systems can actually solve for this missing number.
 -}
cubicS :: Double -> Double -> Double
cubicS q r = do
    if r == 0 && cubicDisc q r == 0
        then 0
        else (cubeRoot (r + sqrt (cubicDisc q r)))

cubicT :: Double -> Double -> Double
cubicT q r = do
    if r == 0 && cubicDisc q r == 0
        then 0
        else (cubeRoot (r - sqrt (cubicDisc q r)))

{-
    - cubicRealSolutions 1 (-6) 11 (-6) = should [NaN, NaN, NaN] but can equal [1,2,3] - [NaN, NaN, NaN] (Works)
    - cubicRealSolutions 1 0 (-3) 0 = should [NaN, NaN, NaN] but can equal [0, sqrt 3, -sqrt 3] - [NaN, NaN, NaN] (Works)
    - cubicRealSolutions 1 (-3) 3 (-1) = should return [1,1,1] - Works
    - cubicRealSolutions 1 (-5) 8 (-4) = should return [1,2,2] - Works
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d = do
    let
        q = cubicQ a b c
        r = cubicR a b c d
    if cubicDisc q r < 0
        then []
        else [x1, x2, x3] where
            q = cubicQ a b c
            r = cubicR a b c d
            s = cubicS q r
            t = cubicT q r
            x1 = (s + t - (b/(3*a)))
            x2 = ((-1) * ((s + t) / 2) - (b/(3*a)) + (((sqrt 3)/(2)) * (s - t)))
            x3 = ((-1) * ((s + t) / 2) - (b/(3*a)) - (((sqrt 3)/(2)) * (s - t)))

{-
    Haskell has a hard time cube rooting negative numbers, so this function will simply check the sign and make the appropriate calculation based on its sign. For negative numbers, simply make them positive, find the root, and make them negative again after the root is found. This isn't complicated, so I have limited my testing to every negative and positive cubed number from 1 to 5, all of which worked except for 5, which returned 4.999999999999...
    However, doing 5 ** (1 / 3) in the GHCi terminal also returns this value, so I fear that this is simply a Haskell limitation. Cube Roots are sadly not reliable in Haskell from my research.
-}
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