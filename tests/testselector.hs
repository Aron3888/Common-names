-- tests the selector to produce the correct answer
x_selector2 :: Int
x_selector2 = n10
  where
    n1 = x_unit(selector(19 ,31 ,29 ,16 ,25 ,23 ,28 ,27) ) 0
    n2 = x_unit(not(selector(14 ,16 ,25 ,13 ,31 ,23 ,24 ,27) ) ) n1
    n3 = x_unit(not(selector(14 ,24 ,27 ,13 ,23 ,16 ,17 ,31) ) ) n2
    n4 = x_unit(not(selector(16 ,17 ,23 ,13 ,25 ,14 ,24 ,27) ) ) n3
    n5 = x_unit(not(selector(16 ,27 ,18 ,13 ,25 ,14 ,19 ,31) ) ) n4
    n6 = x_unit(not(selector(17 ,14 ,23 ,13 ,16 ,24 ,25 ,19) ) ) n5
    n7 = x_unit(not(selector(17 ,16 ,14 ,13 ,25 ,19 ,24 ,31) ) ) n6
    n8 = x_unit(not(selector(19 ,14 ,25 ,13 ,16 ,27 ,31 ,24) ) ) n7
    n9 = x_unit(not(selector(19 ,25 ,14 ,13 ,16 ,31 ,24 ,23) ) ) n8
    n10 = x_unit(not(selector(23 ,31 ,14 ,13 ,24 ,27 ,17 ,25) ) ) n9

x_unit x n = if x then n +1 else n
