import Distribution.Simple.Setup (trueArg)
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala t _) = t `elem` ["Moro", "Tarocco", "Sanguinello"]
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum [y | (Portocala x y) <- l, ePortocalaDeSicilia (Portocala x y)]

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = sum [1 | (Mar x y) <- l, y]


test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Caine _ _) = "Woof"
vorbeste (Pisica _) = "Meow"

rasa :: Animal -> Maybe String
rasa (Caine _ r)= Just r
rasa (Pisica _) = Nothing

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M mat) nr = foldr (&&) True [k| L lista <- mat, let k= sum lista==nr]

test_verif1 = verifica (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) 10 == False
test_verif2 = verifica (M [L [2,20,3], L [4,21], L [2,3,6,8,6], L [8,5,3,9]]) 25 == True

doarPozN :: Matrice -> Int -> Bool
doarPozN (M mat ) nr = foldr (&&) True [k| L lista <- mat, length lista==nr, let k = if length (filter(>0) lista) ==nr then True else False]

testPoz1 = doarPozN (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L [1,2,-3], L [4,5], L [2,3,6,8], L [8,5,3]]) 3 == False


corect :: Matrice -> Bool
corect (M mat) = foldr (&&) True (map(\x -> x==(head l)) l) where l=[length lista| L lista <- mat]


testcorect1 = corect (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) == False
testcorect2 = corect (M [L [1,2,3], L [4,5,8], L [3,6,8], L [8,5,3]]) == True
