import Control.Monad
--import Debug.Trace


mpf = 1000000007 :: Int

modProduct2 m a b = if c > m then c `rem` m else c
	where c = a * b

modProduct m  = foldl (modProduct2 m) 1 

combinacao a b = (modProduct mpf [x+1..a] `div` modProduct mpf [1..y]) - 1
	where (x, y) = if (a - b) > b then (a - b, b)
				      else (b, a - b )

val c | c == '#' = 1
      | c == '.' = 0
      | otherwise = error $ "Invalid char: " ++ show c


sublista a b =  drop (a - 1) . take b

subParede :: [[Char]] -> (Int, Int, Int, Int) -> [[Char]]
subParede ps r@(xa, ya, xb, yb) = p --trace (show r ++ "\n" ++ unlines ps ++ "==> \n" ++unlines p)  p
	where p = map (sublista ya yb) . sublista xa xb $ ps

contaParede = sum . map val . concat
contaTodos = length . concat

readInt :: String -> Int
readInt = read

parseRetangulo l = let [xa, ya, xb, yb] = map readInt . words $ l
 		   in (xa, ya, xb, yb)

calculaParede ps r = combinacao (contaTodos p) (contaParede p)
	where p = subParede ps r

main = do [x, _] <- liftM (map readInt . words) getLine
	  ps <- replicateM x getLine
	  rs <- liftM (map parseRetangulo . lines) getContents
	  putStr $ unlines $ map (show . calculaParede ps) rs

