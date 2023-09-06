howmany :: Int -> Int -> Int -> [String] -> Int
howmany 0 n i grid  = 1
howmany q n i []    = 0
howmany q n i grid  = if i == n
                            then 0
                        else if (head (drop i (head grid))) == '0'
                            then howmany (q-1) n 0 (tail (addqueen i 0 0 grid)) + howmany q n (i+1) grid
                        else howmany q n (i+1) grid

alterrow :: Int -> Int -> Int -> Int -> String -> String
alterrow i j k l ""     = ""
alterrow i j k l str    = if head str == '0'
                            then if conflict i j k l
                                then ('-' : (alterrow i j (k+1) l (tail str)))
                                else ('0' : (alterrow i j (k+1) l (tail str)))
                            else ('-' : (alterrow i j (k+1) l (tail str)))

addqueen :: Int -> Int -> Int -> [String] -> [String]
addqueen i j l []   = []
addqueen i j l grid = ((alterrow i j 0 l (head grid)) : (addqueen i j (l+1) (tail grid)))

conflict :: Int -> Int -> Int -> Int -> Bool
conflict i j k l    = if i == k || j == l || i+l == j+k || k+l == i+j
                            then True
                        else if (i == k+1 || k == i+1) && (j == l+2 || l == j+2)
                            then True
                        else if (i == k+2 || k == i+2) && (j == l+1 || l == j+1)
                            then True
                        else False

buildline :: Int -> String
buildline 0 = ""
buildline k = ('0' : (buildline (k-1)))

buildgrid :: Int -> Int -> [String]
buildgrid m 0   = []
buildgrid m n   = ((buildline m) : (buildgrid m (n-1)))

main = do
    input <- getLine
    let n = (read input :: Int)
    print (howmany n n 0 (buildgrid n n))
