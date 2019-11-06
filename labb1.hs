{- Lab 1, 2019
   Authors: Matte o Jojje
   Lab group: ?
 -}
---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative exponent can not be used"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1 


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative exponent can not be used"
power1 n k = product list where 
    list = replicate (fromInteger k) (fromInteger n)

    
-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | k < 0 = error "power: negative exponent can not be used"
           | odd k = n * power2 n (k-1)
           | even k =  power2 (n*n) (div k 2)


-- D -------------------------
{- 
Describe your test cases here
* Test a negative input number (for example -10) to make sure the error is handled
* Test zero to make sure it is always one
* Test using the function with an invalid type (for example Double) to make sure function is not executed
* Test using valid numbers and input types (for example 2, 10, 14)
 -}

-- 2
-- Define our power functions as a list of tuples. Therefor it will be
-- easy to add more power functions and test them in the future by simply
-- adding more test-tuples.
powerFuncs :: [(Integer -> Integer -> Integer, Integer -> Integer -> Integer)] 
powerFuncs = [(power, power1), (power1, power2)]

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = and [(fst powerFunc n k) == (snd powerFunc n k) | powerFunc <- powerFuncs]

--3
testCases :: [(Integer, Integer)]
testCases = [(2, 3), (2, 0), (3,5)]

powerTest :: [(Integer, Integer)] -> Bool
powerTest tests = and [prop_powers (fst testCase) (snd testCase) | testCase <- testCases]

--
prop_powers' = undefined

