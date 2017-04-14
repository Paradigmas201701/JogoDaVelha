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

stop :: Int -> String
stop n = concat ["\ESC[", show n, "m"] -- Interrompe a inserçao de cor

cor :: Simbolo -> String
cor s
        | s == X     = stop 36++" "++show s++" "++stop 0 -- 36 eh a cor ciano
        | s == O     = stop 32++" "++show s++" "++stop 0 -- 32 eh a cor verde

-- |Convenience function for constructing an empty board
tabuleiroVazio :: Tabuleiro
tabuleiroVazio = Tabuleiro [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]

-- |Given either a row, column, or diagonal, it checks whether it is entirely filled with naughts or crosses
trioCheio :: Trio -> Bool
trioCheio ts@[a,b,c] = noLefts && allEqual
	where
		noLefts = foldl (\acc curr -> acc && (isRight curr)) True ts
		allEqual = a == b && b == c

-- |Given a game board, check whether the game is over because someone won
ganhou :: Tabuleiro -> Bool
ganhou t = foldl (\acc curr -> acc || (trioCheio curr)) False ((linhas t) ++ (colunas t) ++ (diagonais t))
