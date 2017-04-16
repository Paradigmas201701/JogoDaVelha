{-# LANGUAGE FlexibleInstances #-}
module JogoDaVelha where

import Data.Either
import Data.Maybe
import Data.List

data Simbolo = X | O
	deriving (Show, Eq) -- deriving faz o data Simbolo virar instancia das classes Show e Eq
                        -- Eq eh uma classe que define igualdades ou diferenças

type Bloco = Either Int Simbolo -- O type Parte pode ser ou um Int ou um Symbol

type Trio = [Bloco] -- Três blocos representam um trio

data Tabuleiro = Tabuleiro Trio Trio Trio -- O tabuleiro eh feito de tres linhas com três blocos cada
    deriving (Eq) -- Instanciando o tabuleiro em Eq

instance Show Tabuleiro where -- Show eh uma classe de Data.Maybe
    show tabuleiro =
        -- unlines transforma um vetor de strings em uma unica string, acrescentanto \n
        unlines . surround "+---+---+---+" . map (concat . surround "|". map showBloco) $ (linhas tabuleiro)
        where
        surround x xs = [x] ++ intersperse x xs ++ [x] -- intersperse acrescenta um elemento x entre os elementos de xs
        showBloco = either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

-- Linhas do Tabuleiro
linhas :: Tabuleiro -> [Trio]
linhas (Tabuleiro x y z) = [x, y, z]

-- Colunas do Tabuleiro
colunas :: Tabuleiro -> [Trio]
colunas (Tabuleiro [a, b, c] [d, e, f] [g, h, i]) = [[a, d, g], [b, e, h], [c, f, i]]

-- Diagonais do Tabuleiro
diagonais :: Tabuleiro -> [Trio]
diagonais (Tabuleiro [a, _, c] [_, e, _] [g, _, i]) = [[a, e, i], [c, e, g]]

stop :: Int -> String
stop n = concat ["\ESC[", show n, "m"] -- Interrompe a inserçao de cor

cor :: Simbolo -> String
cor s
        | s == X     = stop 36++" "++show s++" "++stop 0 -- 36 eh a cor ciano
        | s == O     = stop 32++" "++show s++" "++stop 0 -- 32 eh a cor verde

-- Controi tabuleiro vazio
tabuleiroVazio :: Tabuleiro
tabuleiroVazio = Tabuleiro [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]
-- Left eh pq o X começa o jogo

-- Verifica se uma diagonal, uma linha ou uma coluna estão preenchidos com X ou O
trioCheio :: Trio -> Bool
trioCheio ts@[a,b,c] = semO && blocosIguais -- ts = [a,b,c]
	where
		-- foldl soma todos os elementos da lista
		-- \_ = operador lambda. Ele pode virar qualquer variável ou atributo
		-- Verifica se todos os elementos do trio sao X, se forem retorna True, se não, retorna False
		semO = foldl (\simboloAtual simb -> simboloAtual && (isRight simb)) True ts
		blocosIguais = a == b && b == c

-- Verifica se houve vencedor
ganhou :: Tabuleiro -> Bool
ganhou t = foldl (\trioAtual trio -> trioAtual || (trioCheio trio)) False ((linhas t) ++ (colunas t) ++ (diagonais t))

-- Transforma o tabuleiro em uma Lista para que ele possa ser usado pela função isLeft
tabuleiroLista :: Tabuleiro -> [Bloco]
tabuleiroLista (Tabuleiro x y z) = x ++ y ++ z

-- Jogadas Possíveis no jogo
jogadasPossiveis :: Tabuleiro -> [Bloco]
-- Como o jogador X que realiza a primeira jogada, cabe a vez dele saber se ainda há jogadas possiveis
jogadasPossiveis t = filter isLeft (tabuleiroLista t)

-- Verifica se empatou
empate :: Tabuleiro -> Bool
empate t = length (jogadasPossiveis t) == 0 -- Se o tamanho de jogadasPossiveis == 0 então eh empate

-- Converte de lista para Tabuleiro para que possa ser usado em atualizaTabuleiro
listaTabuleiro :: [Bloco] -> Tabuleiro
listaTabuleiro [a,b,c,d,e,f,g,h,i] = Tabuleiro [a,b,c] [d,e,f] [g,h,i]

-- Atualiza o tabuleiro com o simbolo jogado
atualizaTab :: Tabuleiro -> Bloco -> Bloco -> Tabuleiro
atualizaTab t b1 b2 = listaTabuleiro [if x==b1 then b2 else x | x <- tl]
	where tl = tabuleiroLista t

-- Mostra o jogador que ganhou na tela
ganhador :: Tabuleiro -> String
ganhador t = if length simboloJog > 0 then head simboloJog else "Empatou!"
	where
		trioConfig = ((linhas t) ++ (colunas t) ++ (diagonais t))
		simboloJog = [if a == (Right O) then "Jogador dois Ganhou!" else "Jogador um Ganhou!" | ts@[a,b,c] <- trioConfig, trioCheio ts]
