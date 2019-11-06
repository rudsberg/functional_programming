{- Lab 1, 2019
   Authors: Matte o Jojje
   Lab group: ?
 -}
---------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
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
power1 n k | k < 0 = error "power: negative exponent"
power1 n k = product list where 
    list = replicate (fromInteger k) (fromInteger n)
-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | k < 0 = error "power: negative exponent"
           | odd k = n * power2 n (k-1)
           | even k =  power2 (n*n) (div k 2)


-- D -------------------------
{- 

<Describe your test cases here>

 -}

-- 
prop_powers = undefined

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined

