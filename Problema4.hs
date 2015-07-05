import Data.List
import Data.Function
import Control.Monad
import Debug.Trace
import qualified Data.Map as M


data Carta = Carta Int Int
	deriving Eq

data Jogador = Jogador String
	deriving (Eq, Ord, Show)

checaManilha j (Carta f _ ) = case manilha j of
	Carta 3 _  -> f == 4
	Carta f2 _ -> f2 == f + 1

maior2 j c1@(Carta f1 n1) c2@(Carta f2 n2) = case (checaManilha j c1, checaManilha j c2) of 
	(True, True)   -> n1 `compare` n2
	(False, False) -> f1 `compare` f2
	(True, False ) -> GT
	(False, True)  -> LT

maior2r j (c1,_) (c2,_) = maior2 j c1 c2

vencedorRodada :: Jogo -> [(Carta, Jogador)] -> Maybe Jogador
vencedorRodada j rodada = case sortBy (maior2r j) rodada of
	[] -> Nothing
	(x:[]) -> Just $ snd x
	(a:b:_) -> case maior2 j (fst a) (fst b) of
		EQ -> Nothing
		_  -> Just $ snd a

readNumero n = case n of 
	'4' -> 4
	'5' -> 5
	'6' -> 6
	'7' -> 7
	'Q' -> 8
	'J' -> 9
	'K' -> 10
	'A' -> 11
	'2' -> 12
	'3' -> 13

readNaipe n = case n of 
	'D' -> 4
	'S' -> 3
	'H' -> 2
	'C' -> 1

data Jogo = Jogo { manilha   :: Carta 
		 , jogadores :: M.Map Jogador Int
		 , rodadas   :: [[(Carta, Jogador)]]
		 }

pontuaRodada :: Jogo -> M.Map Jogador Int -> [(Carta,Jogador)] -> M.Map Jogador Int
pontuaRodada j p r = case vencedorRodada j r of
	Nothing -> p
	Just v -> M.adjust (+(-1)) v p

pontuaJogo :: Jogo -> M.Map Jogador Int
pontuaJogo j = foldl (pontuaRodada j) (jogadores j) (rodadas j)



pontuacaoFinal j = vencedorJogo $ M.toList (pontuaJogo j)


vencedorJogo vs = 
	let l = sortBy (compare `on` snd) $ map (\(a, b) -> (a, abs b)) vs 
	in case l of -- traceShow l l of
		[] -> Nothing
		[v] -> Just $ fst v
		(a:b:_) -> if snd a < snd b then Just $ fst a else Nothing

mostraVencedor Nothing = "*"
mostraVencedor (Just (Jogador j)) = j

readCarta :: String -> Carta
readCarta [a, b] = Carta (readNumero a) (readNaipe b)

readInt :: String -> Int
readInt = read

getJogador = do [j, c] <- liftM words getLine
		return $ (Jogador j, readInt c)

getRodada :: [Jogador] -> IO [(Carta, Jogador)]
getRodada js = do rs <- liftM words getLine
		  return $ zip (map readCarta rs) js

main = do [nrI, manI] <- liftM words getLine
	  let nr = readInt nrI
	  let man = readCarta manI
	  js <- replicateM 4 getJogador
	  rs <- replicateM nr (getRodada $ map fst js)
	  let j = Jogo man (M.fromList js) rs
	  putStrLn $ mostraVencedor $ pontuacaoFinal j
	  
	  
