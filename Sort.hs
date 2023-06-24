-------------------SELECTION SORT-------------------------------
-- import Data.List (foldr1)
minLs:: (Ord a, Num a) => [a]->a
minLs = foldr (\x acc -> if x < acc then x else acc) 9999

selectionSort::(Ord a, Num a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = 
      let minElement = minLs xs
          rest = removeFromList minElement xs
      in minElement:selectionSort rest

removeFromList _ [] = []
removeFromList el (x:xs)
    | el == x = removeFromList el xs
    | otherwise = x:removeFromList el xs


---------------------INSERTION SORT----------------------------------------------
insertionSort::(Ord a) => [a]->[a]
insertionSort = foldr insertInList []


insertInList::(Ord a)=> a -> [a]->[a]
insertInList y [] = [y]
insertInList y (x:xs)
  | y < x = y:x:xs
  | otherwise = x:insertInList y xs


------------------BUBBLE SORT-------------------
bubble::(Ord a) => [a]->[a]
bubble [] = []
bubble [x] = [x]
bubble (x1:x2:xs) 
  | x1 > x2 = x2:bubble (x1:xs)
  | otherwise = x1:bubble (x2:xs)


bubbleLoop:: (Ord a) => Int -> [a] -> [a]
bubbleLoop 0 xs = xs
bubbleLoop numOfTimes xs = bubbleLoop (numOfTimes-1) (bubble xs)

bubbleSort::(Ord a) => [a] -> [a]
bubbleSort xs = bubbleLoop (length xs) xs


------------------------------MERGE SORT---------------------------------
merge::(Ord a)=> [a]->[a]->[a]
merge l [] = l
merge [] r = r
merge (x:l) (y:r)
  | x <= y = x:merge l (y:r)
  | otherwise = y: merge (x:l) r

splitAt':: Int -> [a] -> ([a],[a])
splitAt' n xs = (take n xs, drop n xs)
length':: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs


mergeSort:: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
        where
          (left, right) = splitAt' (length' xs `div` 2) xs


----------------------------QUICK SORT--------------------------------------
quickSort::(Ord a) => [a]->[a]
quickSort [] = []
quickSort (x:xs) = 
  let smallerThan = quickSort [a |a<-xs, a < x]
      greaterThan = quickSort [a |a<-xs, a >= x]
  in smallerThan ++ [x] ++ greaterThan