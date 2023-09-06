-- main algorithm: count the number of ways to fit q queens onto the given grid with n columns, starting at position i in the 0th row
howmany :: Int -> Int -> Int -> [String] -> Int
howmany 0 n i grid  = 1    -- there is one way to add 0 super-queens to any board
howmany q n i []    = 0    -- there are zero ways to add any non-zero amount of super-queens to an empty board
howmany q n i grid  = if i == n
                            then 0   -- there must be a super-queen in each row, so reaching the end of the row means there is no hope for continuing the current grid
                        else if (head (drop i (head grid))) == '0'    -- available space in grid is marked with 0
                            then howmany (q-1) n 0 (tail (addqueen i 0 0 grid)) + howmany q n (i+1) grid
                        else howmany q n (i+1) grid

-- update the k l entry of the grid to reflect that a super-queen as been added to position i j
alterrow :: Int -> Int -> Int -> Int -> String -> String
alterrow i j k l ""     = ""
alterrow i j k l str    = if head str == '0'
                            then if conflict i j k l
                                then ('-' : (alterrow i j (k+1) l (tail str)))
                                else ('0' : (alterrow i j (k+1) l (tail str)))
                            else ('-' : (alterrow i j (k+1) l (tail str)))

-- update row l of the grid to reflect that a super-queen as been added to position i j
addqueen :: Int -> Int -> Int -> [String] -> [String]
addqueen i j l []   = []
addqueen i j l grid = ((alterrow i j 0 l (head grid)) : (addqueen i j (l+1) (tail grid)))

-- check if a super-queen at i j sees position k l
conflict :: Int -> Int -> Int -> Int -> Bool
conflict i j k l    = if i == k || j == l || i+l == j+k || k+l == i+j
                            then True
                        else if (i == k+1 || k == i+1) && (j == l+2 || l == j+2)
                            then True
                        else if (i == k+2 || k == i+2) && (j == l+1 || l == j+1)
                            then True
                        else False

-- output a string of k 0s
buildline :: Int -> String
buildline 0 = ""
buildline k = ('0' : (buildline (k-1)))

-- output an m by n grid of 0s
buildgrid :: Int -> Int -> [String]
buildgrid m 0   = []
buildgrid m n   = ((buildline m) : (buildgrid m (n-1)))

main = do
    input <- getLine
    let n = (read input :: Int)
    print (howmany n n 0 (buildgrid n n))
