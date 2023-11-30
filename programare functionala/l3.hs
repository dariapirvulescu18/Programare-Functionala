import Data.Char (digitToInt)
import Data.Char(isDigit)
import Text.XHtml (multiple)
nrVocale :: [String] -> Int
nrVocale [] =0
nrVocale (h:t)
   |h == reverse h = calculVocale h + nrVocale t
   |otherwise = nrVocale t

calculVocale :: String -> Int
calculVocale [] =0
calculVocale (h:t)
    | elem h "aeiouAEIOU" = 1 + calculVocale t
    | otherwise= calculVocale t
-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9

-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]
adauga :: Int -> [Int] -> [Int]
adauga _ [] = []
adauga x (h:t)
   | mod h 2 ==0 = h:x:(adauga x t)
   |otherwise = h:adauga x t

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

-- divizori 4 == [1,2,4]
divi :: Int -> [Int]
divi x =[y | y <-[1,2..x], mod x y ==0]

listadiv :: [Int] -> [[Int]]
listadiv []= []
listadiv (h:t)=divi h:listadiv t
-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]

inInterval :: Int -> Int -> [Int] -> [Int]
inInterval x y []= []
inInterval x y (h:t)
    | h>=x && y>= h = h:inInterval x y t
    |otherwise = inInterval x y t 
-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]

inIntervalC :: Int -> Int -> [Int] -> [Int]
inIntervalC x y l =[z| z<-l, z>=x && z<=y]


pozitivRec:: [Int] -> Int
pozitivRec []=0
pozitivRec (h:t)
    | h>=0 = 1+ pozitivRec t
    |otherwise = pozitivRec t

pozitivC:: [Int] -> Int
pozitivC l =sum [1| x<- l, x>=0]

--pozitive [0,1,-3,-2,8,-1,6] == 3

pozitiiImpare ::[Int] -> [Int]
pozitiiImpare []= []
pozitiiImpare (h:s) = ajutor 0 (h:s)

ajutor::Int -> [Int] ->[Int]
ajutor x []=[]
ajutor x (h:s)
    | mod h 2 == 1 = x:ajutor(x+1) s
    |otherwise = ajutor(x+1) s 

-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]



pozitiiImpareC ::[Int] -> [Int]
pozitiiImpareC l = ajutorC (zip l [0..])

ajutorC::[(Int,Int)] ->[Int]
ajutorC l = [y| (x,y)<-l, mod x 2==1]

multDigits:: [Char]-> Int
multDigits [] = 1
multDigits (h:t)
   | isDigit h = digitToInt h * multDigits t
   | otherwise = multDigits t

multDigitsC:: [Char]-> Int
multDigitsC l= product[digitToInt x| x<- l, isDigit x]
-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

permutari :: Eq a => [a] -> [[a]]
permutari [] = [[]]
permutari l = [x:l2 | (x, poz) <- zip l [1..], l2 <- permutari [e | (e, poz1) <- zip l [1..], poz1 /= poz]]