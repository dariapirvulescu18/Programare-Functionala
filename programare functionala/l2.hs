--1
poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a *( x * x ) + b * x + c
--2
eeny :: Integer -> String
eeny x = if even x
         then "eeny"
         else "meeny"
--3
fizzbuzz :: Integer -> String
fizzbuzz x
  | mod x 15 ==0 = "FizzBizz"
  | mod x 3 ==0 = "Fizz"
  | mod x 5 ==0 = "Bizz"
  | otherwise = " "

fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)

tribonacci :: Integer -> Integer
tribonacci 1 =1
tribonacci 2 =1
tribonacci 3 =2
tribonacci n=
    tribonacci (n-1) + tribonacci (n-2)  + tribonacci (n-3)

-- B(n,k) = B(n-1,k) + B(n-1,k-1)
-- B(n,0) = 1
-- B(0,k) = 0
binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k
    | n==k =1
    | otherwise = binomial (n-1)  k + ( binomial (n-1) k-1)


verifL :: [Int] -> Bool
verifL l = even (length l)

-- takefinal - pentru o listă l dată ca parametru si un număr n, întoarce o listă care con t, ine ultimele n
-- elemente ale listei l. Dacă lista are mai put, in de n elemente, întoarce lista nemodificată.

takefinal :: [Int] -> Int -> [Int]
takefinal [] n = []
takefinal (h:t) n
    | n< length (h:t) = takefinal t n
    | otherwise = h:t

--pt sirul de caractere
takefinals :: [a] -> Int -> [a]
takefinals [] n = []
takefinals (h:t) n
    | n< (length (h:t)) = takefinals t n
    | otherwise = (h:t)

-- remove - pentru o listă s, i un număr n, întoarce lista primită ca parametru din care se s, terge elementul
-- de pe pozit, ia n. (Hint: putet, i folosi funct, iile take s, i drop). Scriet, i s, i prototipul funct, iei
remove ::[a] -> Int -> [a]
remove [] n =[]
remove (h:t) n
   | n==0 = t
   | otherwise = h:(remove t (n-1))

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t

myReplicate :: Double -> a -> [a]
myReplicate 0 v = []
myReplicate n v = v:myReplicate (n-1) v


sumImp :: [Int ] -> Int
sumImp []=0
sumImp (h:t)
    | even h = sumImp t
    | otherwise = h + sumImp t


totalLen :: [String] -> Int
totalLen = undefined
