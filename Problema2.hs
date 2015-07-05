import Data.List
import Control.Monad

data IV = IV Int Int
	deriving Eq



dentro x (IV a b) = x >= a && x <= b

conflito (IV a b) c =  a `dentro` c || b `dentro` c

tamanho (IV a b) | b - a >= 0 = b - a
		 | otherwise  = error $ "Negative Interval: " ++ show (a, b)


instance Ord IV where 
	a `compare` b = tamanho a `compare` tamanho b


somaIvs [] = 0
somaIvs (i:is) = tamanho i + somaIvs is

somaConflitos i = somaIvs . filter (conflito i)

aloca1 alocs [] = alocs
aloca1 alocs (i:is) | any (conflito i) alocs         = aloca1 alocs is
		    | tamanho i < somaConflitos i is = aloca1 alocs is
		    | otherwise              = aloca1 (i:alocs) is

aloca = aloca1 [] . reverse . sort

readInt :: String -> Int
readInt = read

readIV = do x <- getLine
	    let [a, b] = map readInt $ words x
	    return $ IV a b


main = do x <- getLine
	  ivs <- replicateM (read x) readIV 
          putStrLn $ show $ sum $ map tamanho $ aloca ivs


	    
