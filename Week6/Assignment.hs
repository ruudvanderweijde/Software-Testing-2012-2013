module Assignment

where 

import Week6
import Lab6

import Control.Exception
import Control.Monad
import Data.Bits

import System.Random
import System.CPUTime
import System.TimeIt

import Text.Printf

{- Exercise 1:
    Created exM 
-}
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' b e m
    | even e    = multM z z m
    | otherwise = multM b (multM z z m) m
    where z = exM' b (div e 2) m

-- IMPLEMENTED JORRYT HIS FUNCTION TO TEST THE SPEED!!!
-- Improved exM function using right to left binary method
exMJorryt :: Integer -> Integer -> Integer -> Integer
exMJorryt base expo modulus  = rightToLeftBinary 1 base expo modulus
            where rightToLeftBinary :: Integer -> Integer -> Integer -> Integer -> Integer
                  rightToLeftBinary result base expo modulus
                    | (expo > 0) = rightToLeftBinary newResult newBase newExponent modulus -- recurse until expo is 0
                    | otherwise = result
                    where newResult = if ((.&.) expo 1 == 1) -- Check for odd number
                                      then mod (result * base) modulus -- Intermediate result
                                      else result
                          newExponent = shiftR expo 1 -- Bit shift (dev by 2 basically)
                          newBase = mod (base * base) modulus

--from: http://rosettacode.org/wiki/Modular_exponentiation#Haskell
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

-- simple test to check the logic
test1 = all test1' [0..10000]
test1' x = (x^33) `mod` 5 == (x^32 `mod` 5) * (x `mod` 5)
test2 = all test2' [0..10000]
test2' x = (x^33) `mod` 5 == (rem ((x^16 `mod` 5) * (x^16 `mod` 5)) 5) * (x `mod` 5)

{-- Measure functions: Results below -}
testNumbersStr = ["m1,m2,m3","m2,m3,m4","m3,m4,m5","m4,m5,m6","m5,m6,m7","m6,m7,m8","m7,m8,m9","m8,m9,m10","m9,m10,m11","m10,m11,m12","m11,m12,m13","m12,m13,m14","m13,m14,m15","m14,m15,m16","m15,m16,m17","m16,m17,m18","m17,m18,m19","m18,m19,m20","m19,m20,m21","m20,m21,m22","m21,m22,m23","m22,m23,m24","m23,m24,m25"]
testNumbers    = [[m1,m2,m3],[m2,m3,m4],[m3,m4,m5],[m4,m5,m6],[m5,m6,m7],[m6,m7,m8],[m7,m8,m9],[m8,m9,m10],[m9,m10,m11],[m10,m11,m12],[m11,m12,m13],[m12,m13,m14],[m13,m14,m15],[m14,m15,m16],[m15,m16,m17],[m16,m17,m18],[m17,m18,m19],[m18,m19,m20],[m19,m20,m21],[m20,m21,m22],[m21,m22,m23],[m22,m23,m24],[m23,m24,m25]]
--testNumbersStr' = ["m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26"]
--testNumbers'    = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26]
testNumbersStr' = ["m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26"]
testNumbers'    = [m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26]

genRandomList :: IO (String, [Integer])
genRandomList = genRandomList' testNumbersStr' testNumbers'
    

genRandomList' :: [String] -> [Integer] -> IO (String, [Integer])
genRandomList' xs ys = do
        i <- getRandomInt ((length ys)-1)
        j <- getRandomInt ((length ys)-1)
        k <- getRandomInt ((length ys)-1)
        return ((xs !! i ++ "," ++ xs !! j ++ "," ++ xs !! k),(ys !! i):((ys !! j):[(ys !! k)]))
            


-- getRandomInt creates a random integer between zero and n
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

measureExMs :: IO ()
measureExMs = measureExMs' $ zip testNumbersStr testNumbers

--measureExMsRand = do
--    randList <- genRandomList
--    measureExMs' randList

measureExMRand :: Integer -> IO ()
measureExMRand 0 = putStrLn "All done."
measureExMRand n = do 
        randomList <- genRandomList
        testCorrectNess (fst randomList) (snd randomList)
        measureExM (fst randomList) (snd randomList)
        measureExMRand (n-1)

measureExMs' :: [(String,[Integer])] -> IO ()
measureExMs' []     = putStrLn "All done."
measureExMs' (x:xs) = do 
        testCorrectNess (fst x) (snd x)
        measureExM (fst x) (snd x)
        measureExMs' xs

--measureExM' :: IO ()
--measureExM' = do
--        -- Execute new function + measure the time it took to compute
--         startBinaryMethod <- getCPUTime
--         let binaryMethodResult = exM m23 m24 m25
--         endBinaryMethod <- getCPUTime
--         let computingTimeBinaryMethod = endBinaryMethod - startBinaryMethod
--         --printf "Computation time ExM (Original): %0.10f sec\n" ((fromIntegral computingTimeOriginal) / 10^12 :: Double)
--         printf "Computation time ExM (Binary Method): %0.10f sec\n\n" ((fromIntegral computingTimeBinaryMethod) / 10^12 :: Double)
--         print binaryMethodResult

testCorrectNess :: String -> [Integer] -> IO ()
testCorrectNess s (b:e:m:xs) = do
    --testExM <- exM b e m
    --testExM' <- exM' b e m
    --testExMJorryt <- exMJorryt b e m
    --testPow <- powm b e m 1
    if (testExM /= testExM' || testExM /= testExMJorryt || testExM /= testPow)
    then error "Error test failed!" 
    else putStrLn "Test passed"
    where 
        testExM = exM b e m
        testExM' = exM' b e m
        testExMJorryt = exMJorryt b e m
        testPow = powm b e m 1

-- returns the execution time of the functions
measureExM :: String -> [Integer] -> IO ()
measureExM s (b:e:m:xs) = do
    printf "Test '%s'" s
    --test1 <- timeToString  ((exM b e m) `seq` return ())
    --putStr (" exM:" ++ test1)
    testExM' <- timeToString  ((exM' b e m) `seq` return ())
    putStr (" |exM':" ++ testExM')
    testExMJorryt <- timeToString  ((exMJorryt b e m) `seq` return ())
    putStr (" |exMJorryt:" ++ testExMJorryt)
    testPow <- timeToString  ((powm b e m 1) `seq` return ())
    putStrLn (" |pow:" ++ testPow)
    --if ((read testExM' :: Float) == (read testExMJorryt :: Float)) 
    --then putStrLn " | no winner!"
    --else 
    --    if ((read testExM' :: Float) < (read testExMJorryt :: Float)) 
    --    then putStrLn " | testExM' wins"
    --    else putStrLn " | testExMJorryt wins"
    --if b <= m6 then do
    --    test3 <- timeToString ((expM b e m) `seq` return ())
    --    putStrLn (" | expM:" ++ test3)
    --else 
    --    putStrLn " | expM': Skipped."

{- borrowed this function from haskell.org and modified it.
   Link: http://www.haskell.org/haskellwiki/Timing_computations -}
timeToString :: PrintfType b => IO t -> IO b
timeToString a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return $ printf "%0.5f" (diff :: Double)

{- Exercise 2
    Result of measureExMs:

        Test 'm1,m2,m3' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm2,m3,m4' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm3,m4,m5' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm4,m5,m6' exM: 0.000 sec |exM': 0.000 sec |expM: 0.000 sec
        Test 'm5,m6,m7' exM: 0.000 sec |exM': 0.000 sec |expM: 0.031 sec
        Test 'm6,m7,m8' exM: 0.000 sec |exM': 0.000 sec |expM: 0.281 sec
        Test 'm7,m8,m9' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm8,m9,m10' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm9,m10,m11' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm10,m11,m12' exM: 0.016 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm11,m12,m13' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm12,m13,m14' exM: 0.000 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm13,m14,m15' exM: 0.016 sec |exM': 0.000 sec | expM': Skipped.
        Test 'm14,m15,m16' exM: 0.031 sec |exM': 0.062 sec | expM': Skipped.
        Test 'm15,m16,m17' exM: 0.062 sec |exM': 0.062 sec | expM': Skipped.
        Test 'm16,m17,m18' exM: 0.109 sec |exM': 0.125 sec | expM': Skipped.
        Test 'm17,m18,m19' exM: 0.234 sec |exM': 0.218 sec | expM': Skipped.
        Test 'm18,m19,m20' exM: 0.359 sec |exM': 0.359 sec | expM': Skipped.
        Test 'm19,m20,m21' exM: 1.014 sec |exM': 1.030 sec | expM': Skipped.
        Test 'm20,m21,m22' exM: 2.324 sec |exM': 2.356 sec | expM': Skipped.
        Test 'm21,m22,m23' exM: 3.791 sec |exM': 3.822 sec | expM': Skipped.
        Test 'm22,m23,m24' exM: 8.096 sec |exM': 8.112 sec | expM': Skipped.
        Test 'm23,m24,m25' exM: 16.723 sec |exM': 16.770 sec | expM': Skipped.
        All done.
        (66.16 secs, 1541471992 bytes)
-}

{- Some random code -}

-- test these numbers
primeList :: Integer -> Integer -> [Integer]
primeList n m = [ x | x <- [n..m], isPrime x]

infix 1 ==> 

(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

-- test all numbers, instead of a random number
prime_test_F' :: Integer -> Bool
prime_test_F' 1 = False
prime_test_F' n = prime_test_F'' (n-1) n
     
prime_test_F'' :: Integer -> Integer -> Bool
prime_test_F'' 0 n = True
prime_test_F'' x n = if prime && res
                     then False
                     else prime_test_F'' (x-1) n
        --where res  = exM x (n-1) n /= 1
        -- new according to wikipedia, this is the 'right test'
        where prime = isPrime x 
              res   = exM x n n /= x 

-- test this function for correctness
t1 = all (\x -> prime_test_F' x == isPrime x) [2..1000]

{- Result:
        *Assignment> t1
        True
        *Assignment> all (\x -> isPrime x == prime_test_F' x) [2..5000]
        True
        (51.90 secs, 7000607460 bytes)
-}

{- Exercise 3 -}

composites :: [Integer]
composites = composites' [4..]
composites' (n:ns) = n : composites' 
   (filter (\ m -> head (factors m) /= m) ns)

infiniteElem1 :: (Ord a) => a -> [a] -> Bool
infiniteElem1 x list = (== x) $ head $ dropWhile (< x) list


findCarmichael n = filter (carmichealTest) [2..n]

carmichealTest x = infiniteElem1 x composites &&
                    prime_test_F' x /= isPrime x
{- after modifying the prime_testF'' function: 
      CHANGED: 
         where res  = exM x (n-1) n /= 1
      TO:
         where prime = isPrime x 
         res   = exM x n n /= x 

   Results:
        *Assignment> findCarmichael 1000
        [561]
        (1.39 secs, 207949228 bytes)
        *Assignment> findCarmichael 10000
        [561,1105,1729,2465,2821,6601,8911]
        (117.42 secs, 18728092776 bytes)

   -- Note: Its not very efficient... but it works :-)
-}

{- Exercise 4 -}

-- test using earlier created test function which checks all numbers
testF :: [Integer] -> IO ()
testF [] = print "All done."
testF (k:ks) = do
        isP <- primeF 10 k 
        if isP then do
                print (show k ++ " is marked as prime!")
                testF ks
        else do
                --print (show k ++ " is a not prime!")
                testF ks
        --where isP = prime_test_F' k

testF1 = testF (take 10000 composites)

{- The first one that may fail is 4, because:
        exM 3 3 4 == 3
        exM 2 3 4 == 0
        exM 1 3 4 == 1  <- fails the primeF test!!!

   Running the test multiple times, the function returns many false positives.
   When upgrading 'the attempts x' in "primeF x y" to 10, there are less false positives.
   For example:
        "1105 is a prime!"
        "6601 is a prime!"
        "8911 is a prime!"
        "10585 is a prime!"
        "All done."
        (533.48 secs, 87417424332 bytes)

        Check with:
        any isPrime [1105,6601,8911,10585]
        False

   - Side note: 
   I used the last line (and commented the first line with isP)
        'where isP = prime_test_F' k'
   to test all possible numbers, and test them to 1. 
   This should be equal to isPrime.
-}

{- Exercise 5 -}
testCarmichael = testF carmichael 

{- Result:
        *Assignment> testF carmichael
        "294409 is a prime!"
        "56052361 is a prime!"
        "118901521 is a prime!"
        "172947529 is a prime!"
        "228842209 is a prime!"
        "1299963601 is a prime!"
        "2301745249 is a prime!"
        "9624742921 is a prime!"
        "11346205609 is a prime!"
        "13079177569 is a prime!"
        "21515221081 is a prime!"
        "27278026129 is a prime!"
        "65700513721 is a prime!"
        "71171308081 is a prime!"
        "100264053529 is a prime!"
        "168003672409 is a prime!"
        "172018713961 is a prime!"
        "173032371289 is a prime!"
        "464052305161 is a prime!"
        "527519713969 is a prime!"
        "663805468801 is a prime!"
        "727993807201 is a prime!"
        "856666552249 is a prime!"
        etc... etc...

        testCarmichael
        "56052361 is a prime!"
        "118901521 is a prime!"
        "172947529 is a prime!"
        "216821881 is a prime!"
        "228842209 is a prime!"
        "1299963601 is a prime!"
        "2301745249 is a prime!"
        "9624742921 is a prime!"
        "11346205609 is a prime!"
        "13079177569 is a prime!"
        etc... slightly different numbers



        map isPrime carmichael
        [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,Interrupted
        map prime_test_F' carmichael
        [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,Interrupted.
        
        After reading the carmichael on wikipedia, I still not produced the same list.
-}

{- Exercise 6 -}
testMR :: Int -> [Integer] -> IO ()
testMR _ [] = print "All done."
testMR n (k:ks) = do
        isP <- primeMR n k 
        if isP then do
                print (show k ++ " is marked as prime!")
                testMR n ks
        else do
                --print (show k ++ " is a not prime!")
                testMR n ks

{-
    Test result on composites with 1 attempt
        *Assignment> testMR 1 (take 100 composites)
        "4 is marked as prime!"
        "49 is marked as prime!"
        "85 is marked as prime!"
        "All done."
        (0.09 secs, 7763404 bytes)
        *Assignment> testMR 1 (take 100 composites)
        "9 is marked as prime!"
        "All done."
        (0.00 secs, 515484 bytes)
        *Assignment> testMR 1 (take 100 composites)
        "15 is marked as prime!"
        "39 is marked as prime!"
        "55 is marked as prime!"
        "All done."

    Test results on composites with 10 attempts:
        *Assignment> testMR 10 (take 100 composites)
        "All done."
        (0.02 secs, 1102924 bytes)
    etc...  (same results over and over again)

    Ok, now the tests with Carmichael numbers, using 1 attempt:
        *Assignment> testMR 1 (take 100 carmichael)
        "1299963601 is marked as prime!"
        "21515221081 is marked as prime!"
        "1396066334401 is marked as prime!"
        "3719466204049 is marked as prime!"
        "8544361005001 is marked as prime!"
        "67858397221969 is marked as prime!"
        "73103085605161 is marked as prime!"
        "All done."
        (0.12 secs, 12451072 bytes)
        *Assignment> testMR 1 (take 100 carmichael)
        "118901521 is marked as prime!"
        "1299963601 is marked as prime!"
        "1544001719761 is marked as prime!"
        "9332984447209 is marked as prime!"
        "39782913594409 is marked as prime!"
        "48336382727569 is marked as prime!"
        "70895483772049 is marked as prime!"
        "91968282854641 is marked as prime!"
        "195809339861929 is marked as prime!"
        "All done."
        (0.11 secs, 12461572 bytes)

     Same test with Carmichael numbers, but now using 10 attempt:
        *Assignment> testMR 10 (take 100 carmichael)
        "All done."
        (0.12 secs, 14014064 bytes)
        *Assignment> testMR 10 (take 100 carmichael)
        "All done."
        (0.17 secs, 13495536 bytes)
    etc...  (same results over and over again)
-}

{- Exercise 7 -}

-- list of mersene numbers (extracted from my defined list earlier)
mersenne = map head testNumbers

-- get a (big) prime number
firstPrimeAfter n = head [ x | x <- [n..], isPrime x]


testMersenne n = do
        b <- primeMR 10 m
        if b
                then print "prime!";
                else print "no prime!";
        where p = firstPrimeAfter n
              m = ((2^p) - 1)

{- results:
        *Assignment> testMersenne 100
        "no prime!"
        (0.02 secs, 546476 bytes)
        *Assignment> testMersenne 101
        "no prime!"
        (0.02 secs, 0 bytes)
        *Assignment> testMersenne 1000
        "no prime!"
        (0.05 secs, 1636772 bytes)
        *Assignment> testMersenne 10000
        "no prime!"
        (6.07 secs, 89611920 bytes)
        *Assignment> testMersenne 10000
        ________________________________

        *Assignment> testMersenne 5
        "prime!"
        (0.02 secs, 0 bytes)
        *Assignment> testMersenne 6
        "prime!"
        (0.02 secs, 516164 bytes)
        *Assignment> testMersenne 7
        "prime!"
        (0.00 secs, 553140 bytes)
        *Assignment> testMersenne 8
        "no prime!"
        (0.02 secs, 551464 bytes)
        *Assignment> testMersenne 9
        "no prime!"
        (0.00 secs, 550552 bytes)

        *Assignment> testMersenne 50000
        "no prime!"
        (186.59 secs, 2083636836 bytes)
-}

testMersennes n 0 = print "All done."
testMersennes n i = do
        b <- primeMR 10 m
        when (b) $ putStrLn (show m ++ ",")
        testMersennes (p+1) (i-1)
        where p = firstPrimeAfter n
              m = ((2^p) - 1)

{- Test results of testMersennes (first prime, number of next tries on testMersennes)

        *Assignment> testMersennes 1 100
        3,
        7,
        31,
        127,
        8191,
        131071,
        524287,
        2147483647,
        2305843009213693951,
        618970019642690137449562111,
        162259276829213363391578010288127,
        170141183460469231731687303715884105727,
        6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151,
        "All done."
        (0.34 secs, 52462300 bytes)

        *Assignment> take 13 mersenne
        [3,7,31,127,8191,131071,524287,2147483647,2305843009213693951,618970019642690137449562111,162259276829213363391578010288127,170141183460469231731687303715884105727,6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151]

        These are the same numbers.
-}