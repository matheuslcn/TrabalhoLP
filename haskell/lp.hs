pegaPrimeiro :: (a, b, c) -> a
pegaPrimeiro (x,_,_) = x
pegaSegundo :: (a, b, c) -> b
pegaSegundo (_,x,_) = x
pegaTerceiro :: (a, b, c) -> c
pegaTerceiro (_,_,x) = x

pulaNo :: [(Int, Int, Char)] -> Int -> [(Int, Int,Char)]
pulaNo x y
    | (null x) = []
    | (pegaPrimeiro ( head x) == y ) = x
    | otherwise = pulaNo ( tail x ) y


--exempo [(1,2,'a'),(1,3,'b'),(2,4,'a'),(4,-1,'a')] ";(a,a,a)"


procuraProx :: [(Int, Int, Char)] -> (Int, Int, Char) -> [(Int, Int,Char)]
procuraProx x y 
    | (null x) = []
    | (pegaPrimeiro(head x )) == (pegaPrimeiro y) = x
    | otherwise = procuraProx (tail x) y

finalDosParenteses :: String -> Int -> String
finalDosParenteses a 0
    | not (null a) && head a == ',' = tail a
    | otherwise = a
finalDosParenteses a x
    | head a == '(' = finalDosParenteses (tail a) (x+1)
    | head a == ')' = finalDosParenteses (tail a) (x-1)
    | otherwise = finalDosParenteses (tail a) x

loop :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
loop x y
    | not (null x) && (pegaTerceiro (head x) == head y) && (head (tail y) == ')') = loop (tail x) y
    | otherwise = do
        let (r, a, b) = anda' x y
        if r then loop a y else (True, x, finalDosParenteses b 1)


escolha :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
escolha x y
    | not (null x) && (pegaTerceiro (head x) == head y) && ((head (tail y) == ',') || (head (tail y) == ')')) = (True, (pulaNo x (pegaSegundo (head x))), finalDosParenteses y 1)
    | not (null x) && (pegaTerceiro (head x) /= head y) && (head (tail y) == ',') = escolha x (drop 2 y)
    | otherwise = do
       let (r, a, prox) = anda' x y
       if r then (r, a, finalDosParenteses prox 1)
       else let (j, i, k) = anda' x prox in (j, i, finalDosParenteses k 1)

sequencial :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
sequencial x y
    | not (null x) && not (null y) && (pegaTerceiro (head x) == head y) && (head (tail y) == ')') = (True, (pulaNo x (pegaSegundo (head x))), finalDosParenteses y 1)
    | not (null x) && not (null y) && (pegaTerceiro (head x) == head y) && (head (tail y) == ',') = sequencial (pulaNo x (pegaSegundo (head x))) (drop 2 y)
    | otherwise = do
        let (r, a, b) = anda' x y
        if r then sequencial a b else (r, a, finalDosParenteses b 1)


anda' :: [(Int, Int, Char)] -> String -> (Bool, [(Int, Int, Char)], String)
anda' a [] = (null a, a, "")
anda' [] a = (null a, [], a)
anda' x y
    | head y == '*' =  do 
        let (r,a,b) = loop x (drop 2 y)
        if not r then anda' (procuraProx (tail x) (head x)) y else (r,a,b) 
    | not (null x) && head y == ';' = do 
        let (r,a,b) = sequencial x (drop 2 y)
        if not r then anda' (procuraProx (tail x) (head x)) y else (r,a,b) 
    | not (null x) && head y == 'U' =  do
        let (r,a,b) = escolha x (drop 2 y)
        if not r then anda' (procuraProx (tail x) (head x)) y else (r,a,b) 
    | otherwise = (False,x,y) 
        
    --do
    --    let (r,a,b) = anda' (procuraProx (tail x) (head x)) y 
    --    if not r then (False,x,y) else (r,a,b)

anda :: [(Int, Int, Char)] -> String -> (Bool,(Int, Int, Char))
anda [] a = (null a, (-1,-1,' '))
anda a [] = (null a, head a)
anda x y = do
    let (resp, a, b) = anda' x y
    if not resp then  anda (procuraProx (tail x) (head x)) y
    else (if ((not(null a) && ((pegaSegundo(head a)) == -1)) || (null a) ) then (resp, (-1,-1,' ')) else anda (procuraProx (tail x) (head x)) y)


-- [(1,2,'a'),(1,3,'b'),(2,4,'a'),(4,-1,'b')] "U(;(a,a,b),;(a,a,a))"


main :: IO ()
main  = do
    putStrLn("coloque o grafo")
    x <- getLine
    putStrLn("coloque a formula")
    y <- getLine
    let (a,b) = anda (read x :: [(Int, Int, Char)]) (read y :: String)
    if a then putStrLn "Eba funcionou"
    else putStrLn(show (pegaPrimeiro b) ++ show (pegaSegundo b) ++ show (pegaTerceiro b))
        