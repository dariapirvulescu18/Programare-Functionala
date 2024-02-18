{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
newtype Identity a = Identity a
    deriving Show

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a
    deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)


data Constant a b = Constant b
    deriving Show

instance Functor (Constant a) where
    fmap f (Constant b ) = Constant (f b)

data Two a b = Two a b
    deriving Show

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c
    deriving Show

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b
    deriving Show

instance Functor (Three' a) where
    fmap f (Three' x y  z) = Three' x (f y)(f z)  

data Four a b c d = Four a b c d
    deriving Show

instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z (f w)

data Four'' a b = Four'' a a a b
    deriving Show

instance Functor (Four'' a ) where
    fmap f (Four'' x y z w ) = Four'' x y z (f w)

data Quant a b = Finance | Desk a | Bloor b
    deriving Show

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk x) = Desk x
    fmap f (Bloor y) = Bloor (f y) 

data LiftItOut f a = LiftItOut (f a)
    deriving Show

instance Functor f =>  Functor (LiftItOut f) where
    fmap h (LiftItOut x) = LiftItOut (fmap  h x)  

data Parappa f g a  = DaWrappa (f a) (g a)
    deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa x y)  = DaWrappa (fmap f x) (fmap f y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving Show

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving Show


instance (Functor g) => Functor (Notorious g o a ) where
    fmap f (Notorious x y z) = Notorious x y (fmap f z)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show


instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x)(fmap f y)(fmap  f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)


instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print x y) = Print x (f y)
    fmap f (Read h) = Read (fmap f h)


-- instance Functor TalkToMe where
--     fmap _ Halt = Halt
--     fmap f (Print x y ) = Print x (f y)
--     fmap f (Read functie ) = Read (f.functie)
