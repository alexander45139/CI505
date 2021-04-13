{- 
   CI505 Introduction to Functional Programming
   In-Class Test 1 
-}
module Main where

import Test.QuickCheck

{- 1. Complete the following function where myTakeWhile p xs returns
elements of xs as a list until it reaches an element of \texttt{xs}
for which \texttt{p} is false. For example, myTakeWhile (\x -> x < 3) [1, 2, 3, 4]
returns [1, 2]. -}

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p []     = []
myTakeWhile p (x:xs) = if p x then x : myTakeWhile p xs else []

{- 2. Find the penultimate (second to last) element in list l. Behaviour is 
  undefined if the list has fewer than 2 elements. -}

penultimate :: [a] -> a
penultimate l = if (length l) < 2 then undefined else l !! ((length l) - 2)

{- 3. Find the element at index k in list l. For example, findK 2 [0,0,1,0,0,0]
returns 1. -}

findK :: Int -> [a] -> a
findK k [] = undefined
findK k l  = if k < (length l) && k >= 0 then l !! k else undefined

{- 4. Determine if a list, l, is a palindrome. -}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = if l == reverse l then True else False

{- 5. Duplicate the elements in list xs. For example duplicate [1,2,3] should
  give the list [1,1,2,2,3,3]. Hint: The concat [l] function flattens a list
  of lists into a single list. For example: concat [[1,2,3],[3,4,5]] returns
  [1,2,3,3,4,5]. -}

duplicate :: [a] -> [a]
duplicate []     = []
duplicate (x:xs) = x : x : duplicate xs

{- 6. Split a list, l, at element k into a tuple containing the first part of
  l up to and including k, followed by the part of l which comes after
  k. For example splitAtIndex 3 [1,1,1,2,2,2] returns
  ([1,1,1,2],[2,2]). -}

splitAtIndex :: Int -> [a] -> ([a], [a])
splitAtIndex _ []     = ([], [])
splitAtIndex 0 (x:xs) = ([x], xs)
splitAtIndex k l      = (take (k + 1) l, drop (k + 1) l)

{- 7. Drop the element at index k in list l. For example dropK 3
    [0,0,0,1,0,0,0] returns [0,0,0,0,0,0]. -}

dropK :: Int -> [a] -> [a]
dropK k [] = []
dropK k l  = if k < (length l) then take (k - 1) l ++ drop k l else []

{- 8. Extract elements between ith and kth element in list l,  including i but not k. 
   For example, slice 3 6 [0,0,0,1,2,3,0,0,0]  returns [1,2,3]. -}

slice :: Int -> Int -> [a] -> [a]
slice i k []     = []
slice i 0 l      = []
slice i k l = take (k - i) (drop i l)

{- 9. Insert element x in list l at index k. For example,
insertElem 2 5 [0,0,0,0,0,0] returns [0,0,0,0,0,2,0]. If index k
 does not exist in l, insert x at the tail of l. -}

insertElem :: a -> Int -> [a] -> [a]
insertElem x k [] = [x]
insertElem x k xs = if k >= (length xs) then xs ++ [x] else (take k xs) ++ [x] ++ (drop k xs)

{- 10. Rotate list l n places left, where n is less than the length of the list. 
   For example, rotate 2 [1,2,3,4,5] gives [3,4,5,1,2]. -}

rotate :: Int -> [a] -> [a]
rotate n [] = []
rotate 0 xs = xs
rotate n xs = drop n xs ++ take n xs

{- TESTS

Run these from the interpreter by invoking quickCheck, e.g.
> quickCheck prop_myTakeWhile
 Or run them all by running the main function :main

-}

prop_myTakeWhile xs = not (null xs) ==> (<3) (head xs) ==> 
                      length xs >= length (myTakeWhile (<3) xs)
                          where types = xs::[Int]
prop_myTakeWhileNot xs = not (null xs) ==> not ((<3) (head xs)) ==> 
                         null (myTakeWhile (<3) xs)
                             where types = xs::[Int]

prop_penultimate :: [Int] -> Property
prop_penultimate xs = length xs > 1 ==> penultimate xs `elem` xs
prop_penultimate2 :: [Int] -> Property
prop_penultimate2 xs = length xs > 1 ==> penultimate xs == xs !! ((length xs)-2)

prop_findK :: Int -> [Int] -> Property
prop_findK k xs = k >= 0 && k < length xs ==> 
                  (findK k xs) `elem` xs

prop_isPalindrome :: [Char] -> Bool
prop_isPalindrome xs = and [isPalindrome [1]
                           , isPalindrome [123454321] ]

prop_duplicate :: [Int] -> Bool
prop_duplicate xs = length (duplicate xs) == (length xs) * 2

prop_splitAtIndex :: Int -> [Int] -> Property
prop_splitAtIndex k xs = k >= 0 ==> ys ++ zs == xs
    where (ys, zs) = splitAtIndex k xs

prop_splitAtIndex2 :: Int -> [Int] -> Property
prop_splitAtIndex2 k xs = k >= 0 && k < length xs ==> length ys == k+1
    where (ys, _) = splitAtIndex k xs

prop_splitAtIndex3 :: Int -> [Int] -> Bool
prop_splitAtIndex3 k xs = length ys + length zs == length xs
    where (ys, zs) = splitAtIndex k xs

prop_dropK :: [Int] -> Bool
prop_dropK xs = dropK 0 xs == xs

prop_dropK2 :: [Int] -> Bool
prop_dropK2 xs = dropK (length xs) xs == []

prop_dropK3 :: [Int] -> Property
prop_dropK3 xs = not (null xs) ==> dropK 1 xs == tail xs

prop_slice :: Int -> Int -> [Int] -> Property
prop_slice i k l = i >= 0 && k >= 0 && i < k ==>
                   slice i k l == take (k-i) (drop i l)

prop_insertElem :: Int -> Int -> [Int] -> Bool
prop_insertElem x i xs = length (insertElem x i xs) == length xs + 1

prop_insertElem2 x i xs = x `elem` (insertElem x i xs) 
prop_insertElem3 x i xs = i >= 0 && i < length xs ==> (insertElem x i xs)!!i' == x
    where i' = if i == 0 then i else i-1

prop_rotate :: Int -> [Int] -> Bool
prop_rotate i xs = (rotate 0 xs) == xs

prop_rotate2 :: Int -> [Int] -> Bool
prop_rotate2 i xs = rotate (length xs) xs == xs

main = do quickCheck prop_myTakeWhile
          quickCheck prop_myTakeWhileNot
          quickCheck prop_penultimate
          quickCheck prop_penultimate2
          quickCheck prop_findK
          quickCheck prop_isPalindrome
          quickCheck prop_duplicate
          quickCheck prop_splitAtIndex
          quickCheck prop_splitAtIndex2
          quickCheck prop_splitAtIndex3
          quickCheck prop_dropK
          quickCheck prop_dropK2
          quickCheck prop_dropK3
          quickCheck prop_slice
          quickCheck prop_insertElem
          quickCheck prop_rotate
          quickCheck prop_rotate2
