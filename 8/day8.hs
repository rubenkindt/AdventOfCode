import Data.List

data Inst = Instr String Int deriving(Show)

part1 :: IO ()
part1 = do
 input <- readFile "day8Input.txt"
 print . executeAndDetectLoop (replicate (length . lines $ input) 0) 0 0 . lineToInstr . lines $ input where

-- print . lineToInstr . lines $ input
-- print $ replicate (length . lines $ input) 0
 

 
lineToInstr :: [String] -> [Inst]
lineToInstr strList = [Instr ((words str)!!0) (read ((words str)!!1)::Int) | str <- strList]

executeAndDetectLoop :: [Int] -> Int -> Int -> [Inst] -> Int
executeAndDetectLoop itterate ip acc instrList = if (newItterate!!ip)== 2 then acc else result where 
 result
  | instruction == "acc" = executeAndDetectLoop (newItterate) (ip + 1) (acc + arg) (instrList)  
  | instruction == "jmp" = executeAndDetectLoop (newItterate) (ip + arg) (acc + 0) (instrList)  -- the if, because i removed the '+' manualy 
  | instruction == "nop" = executeAndDetectLoop (newItterate) (ip + 1)   (acc + 0) (instrList)  --if (isNegative arg) then ip + arg else ip + arg
 (Instr instruction arg) = instrList!!ip
 newItterate = inc itterate ip

isNegative :: Int -> Bool
isNegative int = (int<0) 

inc :: [Int] -> Int -> [Int]
inc (x:xs) 0     = ((x+1):xs)
inc [] _         = error "wrong index"
inc (x:xs) index = (x:(inc xs (index-1)))


part2 :: IO ()
part2 = do
 input <- readFile "day8Input.txt"
 putStrLn "have patience I run slowly"
 print . myItterate . lineToInstr . lines $ input where


myItterate :: [Inst] -> Int
myItterate [] = error "no solution found"
myItterate list = myItterHelper list 0

myItterHelper :: [Inst] -> Int -> Int
myItterHelper instrList index = if infLoop then (myItterHelper instrList (index - 1)) else (execute 0 0 newList)  where
    infLoop =(executeAndDetectLoop2 zeroList 0 0 newList)
    newList  = changeJmpNop instrList index
    zeroList = replicate (length instrList) 0


  
changeJmpNop :: [Inst] -> Int -> [Inst]
changeJmpNop [] _ = error "index out of bounds"
changeJmpNop ((Instr instruction arg):xs) 0 = ((Instr newInstruction arg):xs) where
 newInstruction 
  | instruction =="acc" = "acc"
  | instruction =="jmp" ="nop"
  | instruction =="nop" ="jmp"
changeJmpNop (x:xs) index = (x:(changeJmpNop xs (index + 1)))

execute :: Int -> Int -> [Inst] -> Int
execute ip acc [] = acc 
execute ip acc instrList = if ((length instrList) <= ip) then acc else result where
 result
  | instruction == "acc" = execute (ip + 1) (acc + arg) (instrList)  
  | instruction == "jmp" = execute (ip + arg) (acc + 0) (instrList)
  | instruction == "nop" = execute (ip + 1)   (acc + 0) (instrList) 
  where
   (Instr instruction arg) = instrList!!ip
 
 

executeAndDetectLoop2 :: [Int] -> Int -> Int -> [Inst] -> Bool
executeAndDetectLoop2 itterate ip acc instrList = if ((length instrList) <= ip) then False else (if (newItterate!!ip)>= 5 then True else result )where 
 result
  | instruction == "acc" = executeAndDetectLoop2 (newItterate) (ip + 1) (acc + arg) (instrList)  
  | instruction == "jmp" = executeAndDetectLoop2 (newItterate) (ip + arg) (acc + 0) (instrList)
  | instruction == "nop" = executeAndDetectLoop2 (newItterate) (ip + 1)   (acc + 0) (instrList)
 (Instr instruction arg) = instrList!!ip
 newItterate = inc itterate ip
