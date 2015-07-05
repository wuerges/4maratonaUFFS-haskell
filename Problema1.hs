

divide :: Int -> Int -> Int
divide a b | b > 0 = a `div` b
           | b < 0 = - (a `div` abs b)

resto :: Int -> Int -> Int
resto a b = abs $ a - (b * divide a b)

readInt :: String -> Int
readInt = read


main = do l <- getLine
	  let [a, b] = map readInt $ words l
	  putStrLn $ show (divide a b) ++ " " ++ show (resto a b)
