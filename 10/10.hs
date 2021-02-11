import Data.List
import Data.Char

data Inst = Instr String Int deriving(Show)

example1 :: IO ()
example1 = do
 input <- readFile "day10Example.txt"
 print . multiplOcc . (\(b,x) -> x) . diff . reverse . sort . append0andMax . myStrToint . lines $ input
 
part1 :: IO ()
part1 = do
 input <- readFile "day10Input.txt"
 print . multiplOcc . (\(b,x) -> x) . diff . reverse . sort . append0andMax . myStrToint . lines $ input

append0andMax :: [Int] -> [Int]
append0andMax list = (0:(m+3):list)
 where
  m=maximum list 

multiplOcc :: [Int] -> Int
multiplOcc list = three * one
 where 
  one   = findOccOfOne   list
  three = findOccOfThree list
  

findOccOfOne :: [Int] -> Int
findOccOfOne = (\(nr,occ)->occ) . count 1

findOccOfThree :: [Int] -> Int
findOccOfThree = (\(nr,occ)->occ) . count 3

countOcc :: Int -> [Int] -> (Int,Int)
countOcc nr [] = (nr,0)
countOcc nr (x:xs) = if x==nr then (nr,occ+1) else (nr,occ)
 where 
  (nr,occ) = countOcc nr xs

--modified from https://stackoverflow.com/questions/19554984/haskell-count-occurrences-function
count :: Eq a => a -> [a] -> (a,Int)
count x xs= (x,(length $ filter (x==) xs))

diff :: [Int] -> (Bool,[Int])
diff [x] = (True,[])
diff (x:y:xs) = if continue && 0<dif && dif <= 3 then (continue, (dif:recur)) else (False, recur)
 where
  (continue,recur) = diff (y:xs)
  dif = x-y
  

{-
findDiffAndInc :: Int -> [(Int,Int)]-> [(Int,Int)]
findDiffAndInc x  (nr,freq)     = if x==nr then (nr,freq+1):xs else error "beep"
findDiffAndInc x ((nr,freq):xs) = if x==nr then (nr,freq+1):xs else (findDiffAndInc x xs):xs
-}

myStrToint :: [String] -> [Int]
myStrToint = map (\x -> base10 $ map digitToInt x)

base10 :: [Int] -> Int
base10 = foldl (\acc x -> acc * 10 + x) 0