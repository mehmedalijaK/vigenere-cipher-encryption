import Data.Char

encoder :: String -> String -> String
encoder x y = encoder' (map toUpper x) (cycle (map toUpper y))

encoder' :: String -> String -> String
encoder' [] _ = []
encoder' (x:xs) (y:ys)
                | ord newChar > 90 = chr (ord newChar - 26) : encoder' xs ys
                | otherwise = newChar : encoder' xs ys
                where newChar = chr (ord x + (ord y - 65) `mod` 26)

decoder :: String -> String -> String
decoder x y = decoder' (map toUpper x) (cycle (map toUpper y))

decoder' :: String -> String -> String
decoder' [] _ = []
decoder' (x:xs) (y:ys)
                | ord newChar < 65 = chr (ord newChar + 26) : decoder' xs ys
                | otherwise = newChar : decoder' xs ys
                where newChar = chr (65 + (ord x - ord y) `mod` 26)
