anda' :: (Int, Int, Char) -> String -> (Bool, String)
anda' a b = (True, "ixtringui")


anda :: [(Int, Int, Char)] -> String -> Bool
anda [] a = not (null a)
anda a [] = not (null a)
anda x y = do
    let (resp, prox) = anda' (head x) y
    if resp then anda (tail x) prox
    else resp