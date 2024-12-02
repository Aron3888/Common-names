import Data.List (nub, intersect)

-- run the code
main :: IO ()
main = print (filter selector generator)

-- ensures three different digits in a number
n3D :: Int -> Bool
n3D n = let n2 = n * n in length (digits n2) == 3 && length (nub (digits n2)) == 3

-- converts a number into its digits
digits :: Int -> [Int]
digits n = map (read . (:[])) (show n)

--generates all 8-tuples of unique numbers from 10 to 31
generator :: [(Int, Int, Int, Int, Int, Int, Int, Int)]
generator = [(a1, a2, a3, a4, a5, a6, a7, a8) |
    a1 <- ar,
    a2 <- ar, a2 /= a1,
    a3 <- ar, a3 /= a1, a3 /= a2,
    a4 <- ar, a4 /= a1, a4 /= a2, a4 /= a3,
    a5 <- ar, a5 /= a1, a5 /= a2, a5 /= a3, a5 /= a4,
    a6 <- ar, a6 /= a1, a6 /= a2, a6 /= a3, a6 /= a4, a6 /= a5,
    a7 <- ar, a7 /= a1, a7 /= a2, a7 /= a3, a7 /= a4, a7 /= a5, a7 /= a6,
    a8 <- ar, a8 /= a1, a8 /= a2, a8 /= a3, a8 /= a4, a8 /= a5, a8 /= a6, a8 /= a7,
    a4 == minimum [a1, a2, a3, a4, a5, a6, a7, a8]]
  where
    ar = filter n3D [10..31]
    
-- filters tuples based on relationships between their elements
selector :: (Int, Int, Int, Int, Int, Int, Int, Int) -> Bool
selector (a1, a2, a3, a4, a5, a6, a7, a8) =
  x [(a1, "alan"), (a2, "cary"), (a3, "james"), (a4, "lucy"), (a5, "nick"), (a6, "ricky"), (a7, "steve"), (a8, "victor")]
  where
    x [] = True
    x ((age1, n1):xs) =
      all (\(age2, n2) -> comp n1 n2 age1 age2) xs && x xs
    
    comp :: String -> String -> Int -> Int -> Bool
    comp n1 n2 age1 age2 =
      let sq1 = digits (age1^2)
          sq2 = digits (age2^2)
          sletter = not (null (n1 `intersect` n2))
          sdigit = not (null (sq1 `intersect` sq2))
      in sletter == sdigit
