import Data.Char
import GHC.ExecutionStack (Location(functionName))
import System.Win32 (xBUTTON1)
import Data.Sequence (Seq(Empty))

myInt = 31415926535897932384626433832795028841971693993751058209749445923

maxim :: Integer -> Integer -> Integer
maxim = max
maxim3 x y z =
    let
        u = maxim x y
    in
        maxim u z

test :: (Integer -> Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Bool
test maxim3 a b c =
                a >= maxim3 a b c || b>= maxim3 a b c || c>= maxim3 a b c

suma:: Num a => a-> a ->a
suma x y = x^2+y^2

-- f::Int->[Char]
-- f x= if even x then "par" else "imp"

fact ::Int->Int
fact 0=1
fact n= n*fact (n-1)

maxi::[Int]->Int
maxi = foldr max 0

y=[3..6]

eny::Int ->String
eny x= if even x then"par" else "impar"

fizz::Integer -> String
fizz x
    | mod x 15==0 = "fizzbuzz"
    | mod x 3==0 ="buzz"
    | mod x 5==0 = "fizz"
    | otherwise = " "

tribonaci ::Integer ->Integer
tribonaci n
        | n<=2 = 1
        | n==3 =2
        |otherwise = tribonaci (n-1) +tribonaci (n-2) + tribonaci (n-3)

binomial ::Int ->Int ->Int
binomial n 0=1
binomial 0 k =0
binomial n k =binomial (n-1) k +binomial (n-1) k-1

v::[Int]->Bool
v l =  even (length l)

takefinal ::[Int] ->Int ->[Int]
takefinal [] n=[]
takefinal (h:t) n
    | n< length (h:t) = takefinal t n
    |otherwise = h:t

rremove ::[Int] ->Int ->[Int]
rremove [] n=[]
rremove (h:t) n
    | n==0 =t
    |otherwise = h:rremove t (n-1)

myReplicate::Int ->Int ->[Int]
myReplicate n v
    |n==0 =[]
    |otherwise = v:myReplicate (n-1) v

sumImp::[Int] ->Int
sumImp l=sum [x| x<-l, odd x ]

totalLen::[[Char]]->Int
totalLen l =sum [length x| x<- l,  head x=='A' ]

total::[[Char]]->Int
total []=0
total (h:t)= if head h=='A' then length h + total t else total t

foo1::((Int,Char),String) ->String
foo1 ((x,y),z)="a"

a :: ( Int , String ) -> String
a ( n , s ) = take n s

foo3 ::Int ->Char->(String,Char) ->Int->String
foo3 a b (c,d) e ="a"

palindrom::String ->Bool
palindrom []=True
palindrom l = l==reverse l

-- nrVocale::[String]->Int
-- nrVocale l =sum[length x| x<-l, palindrom x, let x = (filter (`elem`"aeiouAEIUO")x)]    

nrVocale::[String]->Int
nrVocale l =sum [length  (filter (`elem`"aeiouAEIUO") x)| x<-l, palindrom x]


f::Int ->[Int]->[Int]
f n []=[]
f n (h:t)
    |even h =h:n:f n t
    |otherwise = h:f n t

divi::Int->[Int]
divi n =[x| x<-[1..n], mod n x ==0]

listadiv::[Int]->[[Int]]
listadiv l =[lista| nr<-l,let lista =divi nr]

inIntervalRec::Int->Int ->[Int]->[Int]
inIntervalRec _ _ []=[]
inIntervalRec a b (h:t)
        | elem h [a..b] =h:inIntervalRec a b t
        |otherwise = inIntervalRec a b t


inIntervalComp::Int->Int ->[Int]->[Int]
inIntervalComp a b l=[x|x<-l,x `elem` [a..b]]

-- pozitive::[Int]->Int
-- pozitive l=length(filter(>0)l )


-- pozitive::[Int]->Int
-- pozitive l=sum[1|x<-l , x>0]

pozitive::[Int]->Int
pozitive []=0
pozitive(h:t)
        |h>0=1+pozitive t
        |otherwise = pozitive t


-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

-- pozImp::[Int]->[Int]
-- pozImp l=[x|(x,y) <- zip [0..] l , mod y 2==1]


multDigits::String ->Int
multDigits s= product (map digitToInt (filter isDigit s))

factori::Int ->[Int]
factori n=[x| x<- [1..n], mod n x ==0]

prim::Int->Bool
prim n= length (factori n) ==2


numerePrime::Int->[Int]
numerePrime n=[x|x<-[2..n],prim x]

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myzip3 xs ys zs

firstEl::[(a,b)]->[a]
firstEl = map fst

prel2 ::[Int]->[Int]
prel2 =map (\x-> if even x then x*2 else x^2)


functie::Char ->[String]->[String]
functie a = filter (elem a )

fct::[Int]->[Int]
fct l= map (^2) (filter odd l)


fctimp::[Int]->[Int]
fctimp l= map (\(x,i) -> x^2 ) (filter (\(x,i)-> even x) ( zip [1..] l))

eliminaConsoane :: [String] -> [String]
eliminaConsoane  = map (filter (`notElem` "aeiouAEIOU"))

fs::[Int] ->Int
fs l= foldr(+) 0 $ map(^2) $ filter(>0)  l

compose::[a->a]->(a->a)
compose l  = foldr (.) id l

-- (.) este functia dpt foldr
-- si id este pasul de baza

functien ::[[Int]] ->[Int]
functien l = map(+1) (foldr (++) [] l)

-- data Colors = Green | Blue | Read | Add Colors Colors
--     deriving Show
-- culoare :: Colors -> String
-- culoare Green = "verde"
-- culoare Read = "rosu"
-- culoare Blue = "albastru"

-- folo :: [Colors] -> [String]
-- folo [] = []
-- folo (h:t)
--     | h == (Add x y) = 


z = [q, 8] where q = 10 


data Country = Name | Capital


data Price = Price Integer
data Airline = PapuAir | Catapults | TakeYourChancesUnited
data Manufacturer = Mini | Mazda | Tata
data Vehicle = Car Manufacturer Price 


data StrInt = String|Int


fff::StrInt->Bool
fff String =False
fff Int =True

data Dogg a = H a |M a

h x = let g x =x+1 in x+g x


-- data Tree  = Empty |Branch Tree Tree |Leaf
data Tree2 a = Empty| Branch a (Tree2 a) (Tree2 a) |Leaf

data Arb a = Frunza|Node a(Arb a) (Arb a)


f4 :: (a -> b -> c) -> b -> c
f4 h b = h undefined b

f5 list = let (x1,x2):xs =list in x1+x2

op :: [a]->[a]->[a]
op= undefined