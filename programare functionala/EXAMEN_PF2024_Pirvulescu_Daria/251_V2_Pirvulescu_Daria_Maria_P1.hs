import Data.Char


trans::[Char]->[Char]
trans []=[]
trans (h:t)
    |isDigit(h)=h:h:(trans t)
    |elem h "ADT"= trans t
    |toLower(h)==h && isAlpha (h) = '*':(trans t) 
    |otherwise = h:(trans t)
    
--solutie cu monade
trans2::[Char]->[Char]
trans2 xs = do
    h<- xs
    if isDigit(h) then h:[h]
    else 
        if elem h "ADT" then ""
        else 
            if toLower(h)==h && isAlpha (h) then "*"
            else return h

