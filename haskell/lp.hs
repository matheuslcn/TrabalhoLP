loop :: [(Int, Int, Char)] -> String -> Int -> Bool
loop x y z
    | head y == '?' = teste x (drop 2 y) 1
    | head y == 'U' = escolha x (drop 2 y) 1
    | head y == '*' = loop x (drop 2 y) 1
    | head y == ';' = sequencial x (drop 2 y) 1
    | otherwise = False

teste :: [(Int, Int, Char)] -> String -> Int -> Bool
teste x y z
    | head y == '?' = teste x (drop 2 y) 1
    | head y == 'U' = escolha x (drop 2 y) 1
    | head y == '*' = loop x (drop 2 y) 1
    | head y == ';' = sequencial x (drop 2 y) 1
    | otherwise = False

escolha :: [(Int, Int, Char)] -> String -> Int -> Bool
escolha x y z
    | head y == '?' = teste x (drop 2 y) 1
    | head y == 'U' = escolha x (drop 2 y) 1
    | head y == '*' = loop x (drop 2 y) 1
    | head y == ';' = sequencial x (drop 2 y) 1
    | otherwise = False

sequencial :: [(Int, Int, Char)] -> String -> Int -> Bool
sequencial x y z
    | head y == '?' = teste x (drop 2 y) 1
    | head y == 'U' = escolha x (drop 2 y) 1
    | head y == '*' = loop x (drop 2 y) 1
    | head y == ';' = sequencial x (drop 2 y) 1
    | otherwise = False


anda' :: [(Int, Int, Char)] -> String -> Int -> (Bool, String)
anda' a b c = 


anda :: [(Int, Int, Char)] -> String -> Bool
anda [] a = not (null a)
anda a [] = not (null a)
anda x y = resp
    where (resp, _) = anda' x y 0

