import Data.Char


data Bcase =Bcase Int String [Bcase]

input2Bcase :: [[a]] -> [Bcase]
input2Bcase []           = []
input2Bcase ([]:xss)     = []
input2Bcase ((x:[]):xss) = ((Bcase 0 x) []): (input2Bcase xss)
input2Bcase ((x:str:xs):xss) = if (isDigit x) then [((Bcase (digitToInt x) str) (input2Bcase ((xs):xss)))] else (Bcase 0 x) (input2Bcase ((str:xs):[])): (input2Bcase xss)