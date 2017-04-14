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
        unlines . surround "+---+---+---+" . map (concat . surround "|". map showSquare) $ (linhas tabuleiro)
        where
        surround x xs = [x] ++ intersperse x xs ++ [x] -- intersperse acrescenta um elemento x entre os elementos de xs
        showSquare = either (\n -> " " ++ show n ++ " ") (concat . replicate 3 . show)

-- Linhas do Tabuleiro
linhas :: Tabuleiro -> [Trio]
linhas (Tabuleiro x y z) = [x, y, z]

stop :: Int -> String
stop a = concat ["\ESC[", show a, "m"] -- Interrompe a inserçao de cor

color :: Simbolo -> String
color s
        | s == X     = stop 36++" "++show s++" "++stop 0 -- 36 eh a cor ciano
        | s == O     = stop 32++" "++show s++" "++stop 0 -- 32 eh a cor verde

-- |Convenience function for constructing an empty board
tabuleiroVazio :: Tabuleiro
tabuleiroVazio = Tabuleiro [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]
