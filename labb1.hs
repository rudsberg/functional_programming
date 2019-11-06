import Test.QuickCheck

{- Lab 1, 2019
   Authors: Matilda Blomquist och Joel Rudsberg
   Lab group: 55
 -}

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative exponent can not be used"
power n 0  = 1
power n k  = n * power n (k-1)

-- A 
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1 


-- B 
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative exponent can not be used"
power1 n k = product list where 
    list = replicate (fromInteger k) (fromInteger n)

    
-- C 
-- power2
power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | k < 0 = error "power: negative exponent can not be used"
           | odd k = n * power2 n (k-1)
           | even k =  power2 (n*n) (div k 2)


-- D 
-- 1
{- 
Test Cases:
* Test a negative value on n, with an even k, should get a positive answer
* Test a negative value on n, with an odd k, should get a negative answer
* Test n, k = 0, to make sure result is 0
* Test a negative value on k, scince our power functions are not defined for negative k, should throw error
* Test n = 0 with k > 0 to make sure result is always 1
* Test a arbitrary positive value on n with odd and even k, to se if both cases work for power2
* Test using the function with an invalid type (for example Double) to make sure function is not executed 
-}

-- 2
-- Define our power functions as a list of tuples. Therefor it will be
-- easy to add more power functions and test them in the future by simply
-- adding more test-tuples.
powerFuncs :: [(Integer -> Integer -> Integer, Integer -> Integer -> Integer)] 
powerFuncs = [(power, power1), (power1, power2)]

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = and [(fst powerFunc n k) == (snd powerFunc n k) | powerFunc <- powerFuncs]

-- 3
-- All test cases. Simply run "powerTest testCases" to test. If more test cases
-- are desired they can easily be added to list.
testCases :: [(Integer, Integer)]
testCases = [(-5, 4), (-5, 3), (10, 0), (0,23), (5,11), (5,12), (0,0)]

powerTest :: [(Integer, Integer)] -> Bool
powerTest tests = and [prop_powers (fst testCase) (snd testCase) | testCase <- testCases]

-- 4
-- By using absolute value of n and k we will avoid negative Integers which would
-- produce error.
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers (abs n) (abs k)

