ends :: [Int] -> [Int]
ends (x:xs) = x : [last xs]

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = 2 * x : deduzame xs

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2 then x : deduzame2 xs else deduzame2 xs

geraTabela :: Int -> [(Int, Int)]
geraTabela 1 = [(1,1)]
geraTabela n = (n, n^2) : geraTabela (n - 1)

contido :: Char -> String -> Bool
contido letra "" = False
contido letra (s:ss) = letra == s || contido letra ss

translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((x,y):ps) = (x + 2.0, y + 2.0) : translate ps

countLongs :: [String] -> Int
countLongs [] = 0
countLongs (s:ss) = countLongs ss + if length s > 5 then 1 else 0

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (s:ss) = if length s > 5 then s : onlyLongs ss else onlyLongs ss
