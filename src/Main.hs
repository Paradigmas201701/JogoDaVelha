module Main where

import JogoDaVelha
import System.IO

main :: IO ()
main = do
    putStrLn "====== Jogo da Velha ======"

    -- |Grab the user's move, and feed that to tic-tac-toe. Recurse as needed.

-- Funçao para auxiliar que o prompt vai ser feito antes do usuario dar um input
promptLine :: String -> IO String
promptLine text = do
    putStr text
    hFlush stdout
    getLine

-- Mostra o fim do jogo e o ganhador
fimDeJogo :: Tabuleiro -> IO ()
fimDeJogo t = do
	putStrLn (show t)
	putStrLn (winner t)

jogador1 :: Tabuleiro -> IO ()
jogador1 tabuleiro = do
	putStrLn (show tabuleiro)
	posicao <- promptLine "Turno do jogador 1: \nEscolha um bloco para jogar "
	putStrLn ""
	let selecPosicao = Left (read posicao) -- Pega o input do jogador e adiciona X nela
	let novoTab = findAndReplace tabuleiro selecPosicao (Right X)
	if ganhou novoTab || empate novoTab
		then fimDeJogo novoTab
		else jogador2 novoTab

jogador2 :: Tabuleiro -> IO ()
jogador2 tabuleiro = do
	putStrLn (show tabuleiro)
	posicao <- promptLine "Turno do jogador 2: \nEscolha um bloco para jogar "
	putStrLn ""
	let selecPosicao = Right (read loc) -- Pega o input do jogador e adiciona O nela
	let novoBoard = findAndReplace tabuleiro selecPosicao (Left O)
	if ganhou novoTab || empate novoTab
		then fimDeJogo novoTab
		else jogador1 novoTab
