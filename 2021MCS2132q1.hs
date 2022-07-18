{-# LANGUAGE BlockArguments #-}

import Data.Char ( digitToInt, intToDigit )
import Control.Exception ()
import Data.Fixed (mod')
import Data.List (isSubsequenceOf)
import GHC.Base ( foldr, divInt, remInt, quotInt )

stringtodigit :: [Char] -> Int
stringtodigit x =
  if isSubsequenceOf x "0123456789"
    then read x :: Int
    else
      error
        "string Invalid Input exception."
        [0]

chartodigit :: Char -> Int
chartodigit y = stringtodigit [y]

fromString :: [Char] -> [Int]
fromString s =
  case s of
    [] -> []
    (x : xs) -> stringtodigit [x] :fromString xs

stringToRevInt :: [Char] -> [Int]
stringToRevInt s =
  case s of
    [] -> []
    (x : xs) -> stringToRevInt xs ++ [stringtodigit [x]]

lsdigitToString :: Show a => [a] -> [Char]
lsdigitToString s =
  case s of
    [] -> []
    (x : xs) -> show x ++ lsdigitToString xs

revinttostring :: Show a => [a] -> [Char]
revinttostring x =
  case x of
    [] -> []
    x : xs -> revinttostring xs ++ show x



addReverseLs :: [Int] -> [Int] -> [Int]
addReverseLs a b = addReverseLs' a b 0


addReverseLs' :: [Int] -> [Int] -> Int -> [Int]
addReverseLs'  a b carry =
  case (a, b, carry) of
    ([], [], carry) -> [carry | carry /= 0]
    ([], x : xs, carry) -> remInt (x + carry) 10 : addReverseLs'  [] xs (quotInt (x + carry) 10)
    (y : ys, [], carry) -> remInt (y + carry) 10 : addReverseLs'  [] ys (quotInt (y + carry) 10)
    (y : ys, x : xs, carry) -> remInt (x + y + carry) 10 : addReverseLs'   xs ys (quotInt (x + y + carry) 10)

lenList :: (Num b) => [a] -> b
lenList [] = 0
lenList xs = sum [1 | _ <- xs]




digitList :: Int -> [Int]
digitList d = digitListAux d where
    digitListAux 0 = []
    digitListAux d = (d `mod` 10) : digitListAux (d `div` 10)

-- fromString :: [Char] -> [Int]
-- fromString  = map digitToInt


toString::[Int]->[Char]
toString s = case s of
          [] -> ""
          x : xs -> show x ++ toString xs

addList :: [Int] -> [Int] -> [Int]
addList a b = reverse (addReverseLs (reverse a) (reverse b))

lessThan :: Ord a => [a] -> [a] -> Bool
lessThan [] [] = False
lessThan [] [y] = True
lessThan [x] [] = False
lessThan [x] [y] = x<y
lessThan x y =  if length x < length y
                                then True
                              else not (length x > length y) && (
             lastx<lasty || (lastx==lasty && initx `lessThan` inity))
                              where lastx = last x
                                    lasty = last y
                                    initx = init x
                                    inity = init y


-- subtractReversedLs x y = if length x < length y || (x `lessThan` y)
--                     then  reverse $ stripLeadingZeroes $ reverse ( subtractReversed' y x 0)
--                   else reverse $ stripLeadingZeroes $ reverse result
--                     where result = subtractReversed' x y 0

subtractReversedLs x y = if length x < length y || (x `lessThan` y)
                    then   subtractReversed' y x 0
                  else subtractReversed' x y 0

stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs



-- subtractReversed' [] [] z = [z]
subtractReversed' [x] [y] z = [x - z - y]
subtractReversed' [x] [] z = subtractReversed' [x] [0] z
subtractReversed' x [] z = subtractReversed' x [0] z
subtractReversed' (x:xs) (y:ys) z = if x-z>=y
                                  then (x-z-y): subtractReversed' xs ys 0
                                else
                                  (10+x-z-y):subtractReversed' xs ys 1
-- subtractReversed' _ _ z = [z]




makeEqual (l1,l2) = if length l1 < length l2 then
                                makeEqual(0:l1,l2)
                                else if  length l1>length l2 then makeEqual(l1,0:l2)
                                else (l1,l2)


splitls :: [a] -> ([a],[a])
splitls xs = splitAt ((divInt (length xs) 2)) xs

split :: Num a => [a] -> ([a], [a])
split a = makeEqual (splitls a)

-- removepadding :: (Eq a, Num a) => [a] -> [a]
-- removepadding ls  = if head ls == 0 then tail ls
--           else ls

subtractList :: (Num a, Ord a) => [a] -> [a] -> [a]
subtractList a b = removepadding (reverse (subtractReversedLs (reverse a) (reverse b)))


removepadding (0:[]) = [0]
removepadding (0:xs) = removepadding xs
removepadding xs = xs

appendFrontZeroes :: (Eq t, Num t, Num a) => [a] -> t -> [a]
appendFrontZeroes xs  n = if n == 0 then xs else appendFrontZeroes (0:xs) (n-1)
appendBackZeroes :: (Eq t, Num t, Num a) => [a] -> t -> [a]
appendBackZeroes xls n = if n == 0 then xls else appendBackZeroes (xls ++ [0]) (n-1)


countFirstzeros ls c =
  case ls of
    (0:x) -> countFirstzeros x c+1
    _   -> c

countLastzeros ls = countFirstzeros (reverse ls) 0

karatsubals ([],[])  = [0]
karatsubals ([],x:xs) =x:xs
karatsubals (y:ys,[]) =y:ys
karatsubals ([a] ,[b]) =  removepadding((divInt (a*b)  10): [remInt (a*b)  10])
karatsubals (x:xs, y:ys)  = let
                                          -- a = countLastzeros(x:xs)
                                          -- b = countLastzeros(y:ys)
                                          -- h1 =  reverse $ removepadding (reverse (x:xs))
                                          -- h2 = reverse $ removepadding (reverse (x:xs))
                                          m=  divInt (( length (x:xs)) +1)  2
                                          (x1,x0) = split (x:xs)
                                          (y1,y0) = split (y:ys)
                                          z2 = removepadding (karatsubals  (x1,y1))
                                          z0 = removepadding (karatsubals  (x0,y0))
                                          z2andz0 = removepadding (addList z2 z0)
                                          x1andx0 = removepadding (addList x1 x0)
                                          y1andy0 = removepadding (addList y1 y0)
                                          temp = removepadding (karatsubals(x1andx0,y1andy0))
                                          z1 = removepadding (subtractList temp z2andz0)
                                          addz2 = appendBackZeroes z2 (2*m)
                                          numz0 = (length addz2) - (length  z0)
                                          addz0 = appendFrontZeroes z0 numz0
                                          tempz1 = appendBackZeroes z1 m
                                          numz1 =  (length addz2  - length tempz1)
                                          addz1 = appendFrontZeroes tempz1 numz1
                                          z1z0 =removepadding( addList addz0 addz1)
                                          z2z1z0 = removepadding (addList z1z0 addz2)
                                  in
                                     z2z1z0



{-
 as I implemented karatsuba in digit by digit calculation it is very inefficeint but bettter if i chosen fixed size integer blocks to implement karatsuba
-}
karatsuba::[Int]->[Int]->Int->[Int]
karatsuba ls1 ls2 base  = if base == 10 then karatsubals(makeEqual (ls1,ls2) )
                          else [0]


addBinary :: String -> String -> String
addBinary a b = let c = length a - length b
                    d = replicate (abs c) '0'
                    (u, v) = if c > 0 then (a, d ++ b) else (d ++ a, b)
                in  reverse $ addBinary' '0' (reverse u) (reverse v)


addBinary' :: Char -> [Char] -> [Char] -> [Char]
addBinary' c "" "" = if c == '0' then "" else "1"
addBinary' c (x:xs) (y:ys) = let a = digitToInt x + digitToInt y + digitToInt c
                                 (u, v) = if a < 2 then (intToDigit a, '0') else (intToDigit (a - 2), '1')
                             in  u:addBinary' v xs ys



