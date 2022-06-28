pegaPrimeiro :: (a, b, c) -> a
pegaPrimeiro (x,_,_) = x
pegaSegundo :: (a, b, c) -> b
pegaSegundo (_,x,_) = x
pegaTerceiro :: (a, b, c) -> c
pegaTerceiro (_,_,x) = x

finalDosParenteses :: String -> Int -> String
finalDosParenteses a 0 = a
finalDosParenteses a x
    | head a == '(' = finalDosParenteses (tail a) (x+1)
    | head a == ')' = finalDosParenteses (tail a) (x-1)
    | otherwise = finalDosParenteses (tail a) x

loop :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
loop x y
    | not (null x) && (pegaTerceiro (head x) == head y) && (head (tail y) == ')') = loop (tail x) y
    | otherwise = do
        let (r, a, b) = anda' x y
        if r then loop a y else anda' x (finalDosParenteses b 1)

teste :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
teste x y
    | not (null x) && head (tail y) == ')' = (True, x, drop 2 y)
    | otherwise = anda' x y


-- TEM COISA PRA ARRUMAR    
escolha :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
escolha x y = do
       let (r, a, prox) = anda' x y
       if r then (r, a, prox) else anda' x prox

sequencial :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
sequencial x y
    | not (null x) && (pegaTerceiro (head x) == head y) && (head (tail y) == ')') = (True, tail x, drop 2 y)
    | not (null x) && (pegaTerceiro (head x) == head y) && (head (tail y) == ',') = sequencial (tail x) (drop 2 y)
    | otherwise = anda' x y

negacao :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
negacao x y = do
    let (r, a, prox) = anda' x y
    (not r, a, prox)

anda' :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
anda' a [] = (null a, a, "")
anda' x y
    | head y == '*' = loop x (drop 2 y)
    | not (null x) && head y == '?' = teste x (drop 2 y)
    | not (null x) && head y == 'U' = escolha x (drop 2 y)
    | not (null x) && head y == ';' = sequencial x (drop 2 y)
    | not (null x) && head y == '!' = negacao x (drop 2 y)
    | otherwise = (False, x, y)

anda :: [(Int, Int, Char)] -> String -> (Bool,(Int, Int, Char))
anda x y = do
    let (resp, a, b) = anda' x y
    if resp then  anda (tail a) b
    else (resp, head a)
    
main :: IO ()
main  = do
    x <- getLine
    y <- getLine
    let (a,b) = anda (read x :: [(Int, Int, Char)]) (read y :: String)
    if a then putStrLn("Eba funcionou")
    else putStrLn(show (pegaPrimeiro b) ++ show (pegaSegundo b) ++ show (pegaTerceiro b))
        