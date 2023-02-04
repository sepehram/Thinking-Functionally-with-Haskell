import Data.Char

{-
findIndex returns the index of the first appearance
of an item in the list
-}
findInd :: (Eq a) => [a] -> a -> Int -> Int
findInd [] _ _ = error "empty list"
findInd (x:xs) y n = if x == y then n else  findInd xs y (n+1)

findIndex :: (Eq a) => [a] -> a -> Int
findIndex xs x = findInd xs x 0

quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = let 
                    ys = filter (<=x) xs
                    zs = filter (>x) xs
                   in (quicksort ys) ++ [x] ++ (quicksort zs)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = xs ++ (flatten xss)

{-
distinct_items receives a list and returns the list
where all items are distinct (unequal)
-}
distinct_items :: (Eq a) => [a] -> [a]
distinct_items [] = []
distinct_items (x:xs) = let ys = distinct_items xs
                        in if x `elem` ys then ys else x:ys

{-
counter counts how many times an item appears
in a list
-}
counter :: (Eq a) => [a] -> [(a, Int)]
counter [] = []
counter [x] = [(x, 1)]
counter (x:xs) = let ps = counter xs -- ps: count the items in the tail
                     ys = map fst ps -- ys: distinct items in the tail
                 in if x `elem` ys then -- if x is already seen in the tail
                        let i = findIndex ys x -- find x's index
                            preds = take i ps  -- preds: all items in ps before (x, n)
                            succs = drop (i+1) ps -- succs: all items in ps after (x, n)
                            (_, n) = ps !! i -- n: the count of x in the tail
                        in (x, n+1):preds ++ succs -- put updated count of x, i.e., n+1 at the head of the return list 
                                                   --(also remove (x,n) from tail)
                    else -- if x is not already seen
                        (x,1):ps -- put x with count 1 at the head of the return list

{-
filter_words_with_counts receives a number n and a list of word/cout pairs
It returns all pairs whose count are equal to n.
-}
filter_words_with_counts :: Int -> [(String, Int)] -> [(String, Int)]
filter_words_with_counts n xs = filter p xs
                                where p (str, count) = n == count

{-
top_n_words receives a text and a number n.
It returns the top n words in that text.
-}
top_n_words :: String -> Int -> [(String, Int)]
top_n_words str n
    | n <= 0 = []
    | otherwise = let  
                    lcase_str = map toLower str
                    split_str = words lcase_str
                    words_with_count = counter split_str
                    word_counts = map snd words_with_count
                    top_n_counts = take n $ distinct_items $ reverse $ quicksort word_counts
                 in take n $ flatten $ [filter_words_with_counts i words_with_count | i <- top_n_counts]
                 
{-
Number to text converter
-}
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["zeroten", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
hundreds_and_beyond = ["hundred", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion"]

say_upto_9 :: Int -> String
say_upto_9 n
    | n < 0 || n >= 10 = "Not in range: unit"
    | otherwise = units !! n

say_upto_19 :: Int -> String
say_upto_19 n 
    | n < 0 || n >= 20 = "Not in range: teens"
    | n < 10 = say_upto_9 n
    | otherwise = teens !! (n `mod` 10)

say_upto_99 :: Int -> String
say_upto_99 n
    | n < 0 || n >= 100 = "Not in range: tens"
    | n < 20 = say_upto_19 n
    | otherwise = 
        let u = (n `mod` 10)
        in tens !! (n `div` 10) ++ (if u == 0 then "" else " " ++ units !! u)

say_upto_999 :: Int -> String
say_upto_999 n
    | n < 0 || n >= 1000 = "Not in range: [0, 999]"
    | n < 100 = say_upto_99 n
    | otherwise = 
        let 
            hundreds = units !! (n `div` 100)
            t = (n `mod` 100)
        in hundreds ++ " " ++ hundreds_and_beyond !! 0 ++ (if t == 0 then "" else " " ++ say_upto_99 t)

say_beyond_999 :: Int -> String
say_beyond_999 n
    | n < 0 = "Not in range"
    | n <= 999 = say_upto_999 n
    | (n `div` 1000) <= 999 = 
        let
            m = n `div` 1000
            t = n `mod` 1000
        in
            (say_upto_999 m) ++ " " ++ hundreds_and_beyond !! 1 ++ (if t == 0 then "" else " and " ++ say_upto_999 t)
    | (n `div` 1000000) <= 999 = 
        let
            m = n `div` 1000000
            t = (n `mod` 1000000) `div` 1000
            r = n `mod` 1000
        in
            (say_upto_999 m) ++ " " ++ hundreds_and_beyond !! 2 ++ 
                (if t == 0 then "" 
                 else " and " ++ (say_upto_999 t) ++ " " ++ hundreds_and_beyond !! 1 ++ 
                    (if r == 0 then "" 
                    else " and " ++ say_upto_999 r))
     




{-
anagrams of n letter words in text
-}

pair_anagram_with_words :: [String] -> [String] -> [(String, [String])]
pair_anagram_with_words [] [] = []
pair_anagram_with_words (x:xs) (y:ys) =
    let
        paired_list = pair_anagram_with_words xs ys
        paired_list_anagrams = map fst paired_list
    in 
        if x `elem` paired_list_anagrams then
            let i = findIndex paired_list_anagrams x
                preds = take i paired_list
                succs = drop (i+1) paired_list
                (_, wrds) = paired_list !! i
            in preds ++ [(x, wrds ++ [y])] ++ succs
        else
            (x,[y]):paired_list

anagrams :: Int -> String -> [(String, [String])]
anagrams n str 
    | n <= 0 = []
    | otherwise =
        let
            str_words = distinct_items $ words $ map toLower str
            n_letter_words = filter (\w -> length w == n) str_words
            n_letter_anagrams = map quicksort n_letter_words
        in pair_anagram_with_words n_letter_anagrams n_letter_words

