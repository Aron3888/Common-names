-- tests the generator to produce different numbers
x_generator :: Int
x_generator = n10
  where
  n1 = x_unit(elem(14 ,17 ,27 ,13 ,24 ,29 ,23 ,19) generator ) 0
  n2 = x_unit(elem(16 ,24 ,25 ,13 ,19 ,14 ,29 ,23) generator ) n1
  n3 = x_unit(elem(17 ,27 ,28 ,13 ,24 ,23 ,25 ,14) generator ) n2
  n4 = x_unit(elem(24 ,14 ,17 ,13 ,23 ,25 ,16 ,27) generator ) n3
  n5 = x_unit(elem(29 ,28 ,14 ,13 ,23 ,18 ,19 ,16) generator ) n4
  n6 = x_unit(notElem(14 ,24 ,13 ,13 ,27 ,25 ,17 ,29) generator ) n5
  n7 = x_unit(notElem(13 ,31 ,25 ,13 ,17 ,14 ,23 ,27) generator ) n6
  n8 = x_unit(notElem(25 ,17 ,19 ,13 ,25 ,24 ,28 ,29) generator ) n7
  n9 = x_unit(notElem(16 ,19 ,14 ,13 ,18 ,18 ,19 ,28) generator ) n8
  n10 = x_unit(notElem(13 ,14 ,18 ,13 ,25 ,29 ,16 ,25) generator ) n9

x_unit x n = if x then n +1 else n
