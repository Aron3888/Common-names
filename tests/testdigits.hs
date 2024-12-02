-- tests function digits that returns the corresponding list of integers
tester :: Int
tester = n5
  where
    n1 = x_unit(digits 0 == [0]) 0
    n2 = x_unit(digits 5 == [5]) n1
    n3 = x_unit(digits 47 == [4 ,7]) n2
    n4 = x_unit(digits 620 == [6 ,2 ,0]) n3
    n5 = x_unit(digits 2745 == [2 ,7 ,4 ,5]) n4

x_unit x n = if x then n +1 else n
