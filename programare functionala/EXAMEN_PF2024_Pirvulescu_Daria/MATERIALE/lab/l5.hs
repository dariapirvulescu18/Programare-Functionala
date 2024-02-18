import Data.Bits (Bits(xor))
sumaimp :: [Int] -> Int
sumaimp xs = foldr (+) 0 (map (\x -> x^2) (filter (\x -> mod x 2 /= 0) xs))

adevarat::[Bool]->Bool
adevarat t = foldr (&&) True (t)

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies prop l = adevarat (map prop l)

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies prop l = foldr (||) False (map prop l)

mapFoldr:: (a->b) ->[a] ->[b]
mapFoldr f l = foldr (\x acc -> f x :acc) [] l

filterFoldr ::(a->Bool) ->[a] ->[a]
filterFoldr f l = foldr (\x acc -> if f x then x : acc else acc) [] l


listToInt :: [Integer]-> Integer
listToInt lista = foldl (\x acc -> x * 10 + acc) 0 lista


rmChar :: Char -> String -> String
rmChar x l = filter (\c -> c/= x) l

rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec s [] = []
rmCharsRec (h:t) s = rmCharsRec t (rmChar h s)

rmCharsFold :: String -> String -> String
rmCharsFold caractere sir = foldr (\x acc -> rmChar x acc) sir caractere

myReverse ::[Int] -> [Int]
myReverse lista = foldl (\x acc -> acc:x ) [] lista

myReverse2 :: [Int] -> [Int]
myReverse2 lista = foldr (\x y -> y++[x]) [] lista


myUnzip :: [(a, b)] -> ([a], [b])
myUnzip lista = foldr (\(a,b) (la,lb) -> (a:la,b:lb)) ([],[]) lista


union :: [Int] -> [Int] -> [Int]
union l1 l2 = foldl (\acc x -> if elem x acc then acc else x : acc) [] (l1 ++ l2)

union2 ::[Int] ->[Int] ->[Int]
union2 l1 l2 = foldr (\x acc -> if elem x acc then acc else x: acc) [] (l1++l2)

intersect ::[Int] ->[Int] ->[Int]
intersect l1 l2 = foldr (\ x acc -> if elem x l1 && elem x l2 && elem x acc==False then x:acc else acc) [] (l1++l2)

permutationsFold :: [a] -> [[a]]
permutationsFold = foldr (\x perms -> concatMap (insertEverywhere x) perms) [[]]
  where
    insertEverywhere :: a -> [a] -> [[a]]
    insertEverywhere x [] = [[x]]
    insertEverywhere x (y:ys) = (x:y:ys) : map (y:) (insertEverywhere x ys)

myElem ::Int->[Int] ->Bool
myElem x = foldr (\y acc -> x==y || acc) False