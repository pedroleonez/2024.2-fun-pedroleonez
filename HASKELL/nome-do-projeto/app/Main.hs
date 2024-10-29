-- module Main (main) where

-- import Lib

-- Definição do tipo Nat
data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Função para adicionar dois números Nat
add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

-- Função para multiplicar dois números Nat
mult :: Nat -> Nat -> Nat
mult Zero _     = Zero
mult (Succ m) n = add n (mult m n)

-- Função para exponenciação de dois números Nat
expNat :: Nat -> Nat -> Nat
expNat _ Zero     = Succ Zero
expNat m (Succ n) = mult m (expNat m n)

-- Funções auxiliares para facilitar a escrita dos valores Nat
nat1 :: Nat
nat1 = Succ Zero

nat2 :: Nat
nat2 = Succ (Succ Zero)

nat3 :: Nat
nat3 = Succ (Succ (Succ Zero))

nat4 :: Nat
nat4 = Succ (Succ (Succ (Succ Zero)))

nat5 :: Nat
nat5 = Succ (Succ (Succ (Succ (Succ Zero))))

nat6 :: Nat
nat6 = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

nat7 :: Nat
nat7 = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

nat8 :: Nat
nat8 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

nat9 :: Nat
nat9 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))


main :: IO ()
main = print (add nat1 nat2)