pegaPrimeiro :: (a, b, c) -> a
pegaPrimeiro (x,_,_) = x
pegaSegundo :: (a, b, c) -> b
pegaSegundo (_,x,_) = x
pegaTerceiro :: (a, b, c) -> c
pegaTerceiro (_,_,x) = x

loop :: [(Int, Int, Char)] -> String -> (Bool, String)
loop x y
    | (pegaTerceiro (head x) == head y) && (head (tail y) == ')') = loop (tail x) y
--    | x = anda' x y caso o loop n tenha um elemento só
    | otherwise = anda' x y

teste :: [(Int, Int, Char)] -> String -> (Bool, String)
teste x y
    | head y == '?' = teste x (drop 2 y)
    | head y == 'U' = escolha x (drop 2 y)
    | head y == '*' = loop x (drop 2 y)
    | head y == ';' = sequencial x (drop 2 y)
    | otherwise = anda' x y

escolha :: [(Int, Int, Char)] -> String -> (Bool, String)
escolha x y = do
    let (r, prox) = anda' x y
    if r then (r, prox) else anda' x prox

sequencial :: [(Int, Int, Char)] -> String -> (Bool, String)
sequencial x y
    | (pegaTerceiro (head x) == head y) && (head (tail y) == ')') = (True, drop 2 y)
    | (pegaTerceiro (head x) == head y) && (head (tail y) == ',') = sequencial (tail x) (drop 2 y)
    | otherwise = anda' x y

negacao :: [(Int, Int, Char)] -> String -> (Bool, String)
negacao x y = do
    let (r, prox) = anda' x y
    (not r, prox)

anda' :: [(Int, Int, Char)] -> String -> (Bool, String)
anda' [] x = (null x, x)
anda' x [] = (null x, "")
anda' x y
    | head y == '?' = teste x (drop 2 y)
    | head y == 'U' = escolha x (drop 2 y)
    | head y == '*' = loop x (drop 2 y)
    | head y == ';' = sequencial x (drop 2 y)
    | head y == '!' = negacao x (drop 2 y)
    | otherwise = (False, y)

anda :: [(Int, Int, Char)] -> String -> Bool
anda [] a = null a
anda a [] = null a
anda x y = do
    let (resp, prox) = anda' x y
    resp

