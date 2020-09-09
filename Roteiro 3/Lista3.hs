--Exercício 1: Operador lógico OU (pré-fixo):
-- a) Apresente 3 definições para o operador lógico OU, utilizando casamento de padrões.

padrãoOu :: Bool -> Bool -> Bool
padrãoOu True True = True
padrãoOu True False = True
padrãoOu False True = True
padrãoOu False False = False
padrãoOu2 :: Bool -> Bool -> Bool
padrãoOu2 False False = False
padrãoOu2 True _ = True
padrãoOu3 :: Bool -> Bool -> Bool
padrãoOu3 False b = b
padrãoOu3 True _ = True

-- b) Apresente 2 definições para o operador lógico OU, utilizando expressões condicionais (no lugar de casamento de padrões).
condicionalOu4 :: Bool -> Bool -> Bool
condicionalOu4 a b =
  if (a == False && b == False)
    then False
    else True
condicionalOu5 :: Bool -> Bool -> Bool
condicionalOu5 a b =
  if (a /= b)
    then True
    else
      if (a == b && b == False)
        then False
        else True

--Exercício 2:  Defina uma função que recebe dois pontos no espaço e retorna a distância entre eles.
-- Considere que um ponto no espaço é representado por uma dupla de números (float) que correspondem às coordenadas do ponto.

quad :: Float -> Float
quad x = x * x
distPontos :: (Float, Float) -> (Float, Float) -> Float
distPontos (x1, y1) (x2, y2) = sqrt (quad (x2 - x1) + quad (y2 - y1))

--Exercício 3 está como PNG.

--Exercício 4: Dado um valor inteiro, escreva a função recursiva fatorial. 
    --Obs: Fazer uma definição usando guardas e outra com casamento de padrões.

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

--Exercício 5: Dado um número inteiro n, escreva a função recursiva fibo que retorna o n-ésimo termo da sequência de Fibonacci a seguir, sendo os casos base F0 = 0 e F1 = 1. 
--Utilize a definição recursiva vista em sala: fibo(n) = fibo(n-2) + fibo(n-1).

fibo :: Int -> Int
fibo n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fibo (n - 2) + fibo (n - 1)

--Exercício 6: Dado um número inteiro n, escreva a função recursiva n_tri, que retorna o n-ésimo termo da sequência de números triangulares, dada a seguir.

n_tri :: Int -> Int
n_tri n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = n_tri (n - 3) + n_tri (n - 2) + n_tri (n - 1)

-- Exercício 7

caminho :: (Int, Int) -> (Int, Int)
caminho (x, y) = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0,1)
fibo2 n = caminho (fibo2 (n - 1))

--Exercício 8:  Escreva a função potencia2, que calcula a potência de 2 elevada a um expoente n
-- de forma recursiva: 2n = 2n-1 * 2.

potencia2 :: Int -> Int
potencia2 ex
  | ex == 0 = 1
  | otherwise = (2 * potencia2 (ex - 1))

--Exercício 9: 
--a: Escreva a função recursiva prodIntervalo: dados dois inteiros m e n, onde m<n,
--retorna o produto: m*(m+1)*...(n-1)*n.

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
  | m == n = n
  | otherwise = n * prodIntervalo m (n - 1)

--b: Reescreva a função fatorial usando a função prodIntervalo.

fiboProdIntervalo :: Int -> Int
fiboProdIntervalo  n
  | 1 == n = n
  | otherwise = n * fiboProdIntervalo  (n - 1)

--Exercício 10 não foi mandado? 

--Exercício 11: Defina de forma recursiva as funções resto_div e div_inteira, que retornam o
    --resto e o quociente da divisão inteira de um inteiro m por inteiro n, realizando subtrações
    --sucessivas de n a partir de m.
-- Ex: m=20 e n=3: 20-3=17, 17-3=14, 14-3=11, 11-3=8, 8-3=5, 5-3=2.
-- Como 2<3: resto=2 e quociente=6.

resto_div :: Int -> Int -> Int
resto_div numerador denominador =
  if numerador == 0 || denominador == 1
    then 0
    else
      if numerador < denominador
        then numerador
        else resto_div (numerador - denominador) denominador

div_inteira :: Int -> Int -> Int
div_inteira numerador denominador =
  if (numerador < denominador)
    then 0
    else (div_inteira (numerador - denominador) denominador) + 1

--Exercício 12:  Implemente a função mdc, usando a definição recursiva vista em sala:
                --mdc(m,n) = m, se n = 0
                --mdc(m,n) = mdc(n, k), se n > 0, sendo k = m mod n
--Obs: Fazer uma definição usando guardas e outra com casamento de padrões

--a) Usando Guardas:
mdcComGuardas :: (Int, Int) -> Int
mdcComGuardas (m, n)
  | n == 0 = m
  | otherwise = mdcComGuardas (n, (mod m n))

--b) Casamento de Padrões
mdc :: (Int, Int) -> Int
mdc (m, 0) = m
mdc (m, n) = mdc (n, (mod m n))

--Exercício 13:  Implemente a função binomial usando a definição recursiva vista em sala:

--a) Usando Guardas: 

binomialComGuardas :: (Int, Int) -> Int
binomialComGuardas (n, k)
  | k == 0 = 1
  | k == n = 1
  | otherwise = binomialComGuardas (n - 1, k) + binomialComGuardas (n - 1, k - 1)

--b) Casamento de Padrões:
binomial :: (Int, Int) -> Int
binomial (n, 0) = 1
binomial (n, k) =
  if (k == n)
    then 1
    else binomial (n - 1, k) + binomial (n - 1, k - 1)

-- Exercício 14 está como PNG. 

--Exercício 15
--a: Utilizando enumeração, construir uma função que dados dois inteiros a e b construa a
--lista dos inteiros contidos no intervalo fechado [a,b]. Quando a for igual a b, a função
--devolve a lista unitária [a]. Quando a > b a função deverá devolver a lista vazia.

enumeraIntervalo :: Int -> Int -> [Int]
enumeraIntervalo a b 
  |a>b=[]
  |a==b = a:[]
  |otherwise=[a..b]