import Data.List
import qualified Data.Map as M

data Carta = Carta Int Int
	deriving Eq

data Jogador = Jogador String Int

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

data Jogo = Jogo { nRodadas  :: Int
		 , manilha   :: Carta 
		 , jogadores :: M.Map Jogador Int
		 , rodadas   :: [[(Carta, Jogador)]]
		 }


maior1 m [] = []



readCarta :: String -> Carta
readCarta [a, b] = Carta (readNumero a) (readNaipe b)

main = putStrLn "ok"
