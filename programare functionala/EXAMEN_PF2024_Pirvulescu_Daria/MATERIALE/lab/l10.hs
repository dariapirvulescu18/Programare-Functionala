import Data.List (nub)
import Data.Maybe (fromJust)
import System.Win32 (xBUTTON1)

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:
-- 1. (P ∨ Q) ∧ (P ∧ Q)
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
-- 2. (P ∨ Q) ∧ (¬P ∧ ¬Q)
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ( Not (Var  "P") :&: Not( Var "Q"))
-- 3. (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))
p3 :: Prop
p3 = (Var "P" :&:(Var "Q" :|: Var "R")) :&: (( Not (Var  "P") :|: Not( Var "Q")):&:( Not (Var  "P") :|: Not( Var "R")))

instance Show Prop where
  show (Var x) =  x
  show (x :|: y) = "(" ++ show x ++ "|" ++ show y ++ ")"
  show (x :&: y) = "(" ++ show x ++ "&" ++ show y ++ ")"
  show (x :->: y) ="(" ++ show x ++ "->" ++ show y ++ ")"
  show (x :<->: y) ="(" ++ show x ++ "<->" ++ show y ++ ")"
  show(Not x) = "(~"++ show x ++ ")"
test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var x) lista = impureLookup x lista
eval (Not x) lista = not(eval x lista)
eval ( x :|: y) lista = eval x lista || eval y lista
eval (x :&: y) lista = eval x lista && eval y lista
eval (x :->: y) lista = eval (Not x) lista || eval y lista
eval (x :<->: y) lista = eval (x:<->: y) lista && eval (y:<->: x) lista
test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True
test_3 = eval p3 [("P", True), ("Q", False),("R", True)]

variabile :: Prop -> [Nume]
variabile (Var x) = [x]
variabile (Not x) = variabile x
variabile (x :|: y) = nub(variabile x ++ variabile y)
variabile (x :&: y) = nub(variabile x ++ variabile y)
variabile (x :->: y) = nub(variabile x ++ variabile y)
variabile (x :<->: y) = nub(variabile x ++ variabile y)
test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [Env]
envs [] = [[]]
envs (x:xs) = [(x, False): e| e<- envs xs] ++ [(x, True):e| e<-envs xs] 
test_envs = 
    envs ["P", "Q"]
    ==
    [ [ ("P",False), ("Q",False)], [ ("P",False), ("Q",True)], [ ("P",True), ("Q",False)] , [ ("P",True), ("Q",True)]]

satisfiabila :: Prop -> Bool
satisfiabila prop = foldr(||) False([ eval prop envsCurent | envsCurent <- envsProp])
    where 
        variabileProp = variabile prop
        envsProp = envs variabileProp
 
test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool
valida prop = foldr(&&) True([ eval prop envsCurent | envsCurent <- envsProp])
    where 
        variabileProp = variabile prop
        envsProp = envs variabileProp
test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True


echivalenta :: Prop -> Prop -> Bool
echivalenta prop1 prop2 =  eval1==eval2
    where 
        variabileProp1 = variabile prop1
        variabileProp2 = variabile prop2
        variabileTotale = nub(variabileProp1 ++variabileProp2)
        envsTotale = envs variabileTotale
        eval1 = [eval prop1 envsCurent1 | envsCurent1 <-envsTotale]
        eval2 = [eval prop2 envsCurent2 | envsCurent2 <- envsTotale]
 
test_echivalenta1 = True== (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 = False == (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 = True == (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))

