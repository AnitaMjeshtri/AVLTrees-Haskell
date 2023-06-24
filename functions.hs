import qualified Data.Map as Map
filter'::(a->Bool)->[a]->[a]
filter' f = foldr(\x acc -> if f x then x:acc else acc)[]

length'::[a]->Int
length' [] = 0
length' (x:xs) = 1 + length xs

foldr'::(a->a->a)->a->[a]->a
foldr' _ s [] = s
foldr' f s (x:xs) = f x (foldr' f s xs)

foldl'::(a->a->a)->a->[a]->a
foldl' _ s [] = s
foldl' f s (x:xs) = foldl' f (f s x) xs

zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xa) (y:xb) = (f x y):zipWith' f xa xb

reverse'::[a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem'::(Eq a)=>a->[a]->Bool
elem' _ [] = False
elem' a (x:xs) 
  | x == a = True
  | otherwise = elem' a xs

fromList'::(Ord k)=>[(k,v)]->Map.Map k v
fromList' = foldl (\acc (k,v)->Map.insert k v acc) Map.empty

toList'::Map.Map k v->[(k,v)]
toList' = Map.foldrWithKey (\k v acc->(k,v):acc)[]

rotate::[a]->Int->[a]
rotate xs n = let xn = drop n xs
              in (xn ++ take n xs)

packConsecutive::(Eq a)=>[a]->[[a]]
packConsecutive = foldr (\x acc -> case acc of
                            []->[[x]]
                            (y:ys)->if x `elem` y then (x:y):ys else [x]:acc)[]

pack xs = [(head x, length x) | x <- packConsecutive xs]

--prime numbers
isPrime 1 = False
isPrime num = if length([x | x <- [2..num-1], num `mod` x == 0]) == 0 then True else False
isPrimeLs ls = foldr (\x acc -> if isPrime x then x:acc else acc)[] ls

--Insert an element at a given position into a list.
insertAtPos:: (Ord a, Num a, Enum a)=>a->Int->[a]->[a]
insertAtPos el i xs = [x | (x, j)<-zip xs [1..], j<i] ++ [el] ++ drop (i-1) xs
insertAtPos' el 1 xs = el:xs
insertAtPos' el i (x:xs) = x:insertAtPos' el (i-1) xs

elemIndex'::(Eq a) => a -> [a] -> Maybe Int
elemIndex' el xs = helper xs 0
    where
      helper [] _ = Nothing
      helper (x:xs) index 
        | x == el = Just index
        | otherwise = helper xs (index+1)


-- it drops until the condition is not satisfied anymore
dropwhile':: (a -> a -> Bool) -> a -> [a] -> [a]
dropwhile' _ _ [] = []
dropwhile' f a (x:xs)
  | f x a = dropwhile' f a xs
  | otherwise = x:xs


takeWhile':: (a -> a -> Bool) -> a -> [a] -> [a]
takeWhile' _ _ [] = []
takeWhile' f a (x:xs) 
  | f x a = x: takeWhile' f a xs
  | otherwise = []

splitAt':: Int -> [a] -> ([a],[a])
splitAt' n xs = (take n xs, drop n xs)


tail':: [a] -> [a]
tail' [] = []
tail' [x] = []
tail' (x:xs) = xs

last':: [a] -> a
last' xs = xs !! ((length xs)-1)

init'::(Ord a) => [a] -> [a]
init' xs = [x | x<-xs, x/=(xs !! ((length xs)-1))]
init''::(Ord a) => [a] -> [a]
init'' xs = [x | x<-xs, x/=(last' xs)]

--concat
concat':: [[a]]->[a]
concat' xxs = [x | xs<-xxs, x<-xs]
concat'' xxs = foldr (\x acc -> x ++ acc)[] xxs

--intersperse
intersperse'::[a] -> [a] -> [a]
intersperse' [x] el = [x]
intersperse' (x:xs) el = [x] ++ el ++ intersperse' xs el

--intercalate
intercalate':: [a] -> [[a]] -> [a]
intercalate' xs [] = []
intercalate' xs [[x]] = [x]
intercalate' xs (x:xxs) = x ++ xs ++ intercalate' xs xxs

--transpose
transpose':: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]:xs) = xs
transpose' (xs:[]) = [xs]
transpose' xxs = (map head xxs): transpose' (map tail xxs)

nub'::(Eq a) => [a] -> [a]
nub' [] = []
nub' [x] = [x]
nub' (x:xs)
  | x `elem` xs = nub' xs
  |otherwise = x: nub' xs

group'::(Eq a)=> [a]->[[a]]
group' = foldr (\x acc -> case acc of
                []->[[x]]
                (y:ys)->if x `elem` y then (x:y):ys else [x]:acc)[]