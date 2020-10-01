-- ex 1: Usando a função map, escreva a função paridade a seguir que recebe uma
-- lst de inteiros l e retorna uma lst cendo os valores booleanos que indicam
-- a paridade dos elementos de l.

paridade :: Integral a => [a] -> [Bool]
paridade lst = map (even) lst

-- ex2:  Usando a função map, escreva a função prefixos a seguir que recebe uma
-- lst de strings l e retorna uma lst cendo os três primeiros caracteres de
-- cada elemento de l.

prefixos :: [[a]] -> [[a]]
prefixos lst = map (take 3) lst

-- ex 3: Usando a função map, escreva a função saudacao a seguir que recebe uma
-- lst de nomes (strings) l e retorna uma lst cendo cada elemento de l
-- concatenado com a saudação “Oi “ na frente de cada nome

saudacao :: [[Char]] -> [[Char]]
saudacao lst = map ("oi" ++) lst

-- ex 4: Reescreva a definição da função filter que já faz parte da biblioteca
-- padrão do Haskell, chamando-a de filtrar. Além disso, defina a função filtrar
-- usando lst por compreensão.

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar z [] = []
filtrar z (y : ys)
  | z y = y : filtrar z ys
  | otherwise = filtrar z ys

filtrarComCompreensao :: (a -> Bool) -> [a] -> [a]
filtrarComCompreensao cond lst = [z | z <- lst, cond z]

-- ex 5:  Usando a função filter, escreva a função pares que recebe uma lst de
-- inteiros lst e e retorna uma lst cendo os elementos pares de lst.

pares :: Integral a => [a] -> [a]
pares lst = filter (even) lst

-- ex 6: Usando a função filter, escreva a função solucoes a seguir que recebe uma
-- lst de inteiros l e retorna uma lst cendo os valores que satisfazem a equação
-- (5*x + 6) < (x*x). Use uma expressão lambda.

solucoes :: (Ord a, Num a) => [a] -> [a]
solucoes lst = filter (\x -> ((5 * x + 6) < (x * x))) lst

-- (5*x + 6) < (x*x).

-- ex 7:  Usando a função foldr1, escreva a função maior a seguir que recebe uma lst e
-- retorna seu maior elemento.

mmaior :: (Foldable l, Ord a) => l a -> a
mmaior lst = foldr1 max lst

-- ex 8: Usando a função foldr, escreva a função menor_min10 a seguir que recebe
-- uma lst e retorna o menor elemento da lst, desde que este não acima de 10, Se o
-- menor elemento for um valor acima de 10, retorna 10.
--menor_minimo :: [a] -> Int
--menor_minimo lst
-- | min lst > 10 = 10
-- | otherwise = min lst

menor_min10 :: (Foldable t, Ord b, Num b) => t b -> b
menor_min10 lst = foldr (min) 10 lst

-- ex 9:  Usando a função foldr, escreva a função junta_silabasplural a seguir
-- que recebe uma lst de sílabas (strings) e retorna uma palavra (string) formada pela
-- concatenação das sílabas e incluindo um “s” no final .

junta_silabas_plural :: [String] -> String
junta_silabas_plural lst = foldr (++) "s" lst

-- ex 10: teste cada função com os seguintes exemplos de lsts e observe a diferença de desempe
lst1 :: [Integer]
lst1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

lst2 :: [Integer]
lst2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

lst3 :: [Integer]
lst3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

lst4 :: [Integer]
lst4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

lst5 :: [Integer]
lst5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

lst6 :: [Integer]
lst6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

lst7 :: [Integer]
lst7 = [1 .. 1000]

lst8 :: [Integer]
lst8 = [1000, 999 .. 1]

lst9 :: [Integer]
lst9 = lst1 ++ [0]

lst10 :: [Integer]
lst10 = [0] ++ lst3

lst11 :: [Integer]
lst11 = lst1 ++ [0] ++ lst3

lst12 :: [Integer]
lst12 = lst3 ++ [0] ++ lst1

-- ex 10: ) Implemente as funções de ordenação bubblesort, selectionsort,
-- insertionsort e quicksort, conforme as definições apresentadas no
-- material de aula (ordenação crescente).

--Bubble
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble lst = bolhaOrd lst (length lst)

bolhaOrd :: (Num t, Ord a, Eq t) => [a] -> t -> [a]
bolhaOrd lst 0 = lst
bolhaOrd lst n = bolhaOrd (troca lst) (n -1)

troca :: Ord a => [a] -> [a]
troca [x] = [x]
troca (x : y : zs)
  | x > y = y : troca (x : zs)
  | otherwise = x : troca (y : zs)

-- Selection
selection :: Ord a => [a] -> [a]
selection [] = []
selection xs = [x] ++ selection (remove x xs)
  where
    x = minimo xs

remove :: Eq t => t -> [t] -> [t]
remove a [] = []
remove a (x : xs)
  | a == x = xs
  | otherwise = x : (remove a xs)

minimo :: Ord a => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x : xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs

--Insertion
insertion :: (Ord a) => [a] -> [a]
insertion [] = []
insertion (x : xs) = insereOrd x (insertion xs)

insereOrd :: Ord t => t -> [t] -> [t]
insereOrd x [] = [x]
insereOrd x (y : ys)
  | x <= y = (x : y : ys)
  | otherwise = y : (insereOrd x ys)

--Quick
quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (s : xs) = quick [x | x <- xs, x < s] ++ [s] ++ quick [x | x <- xs, x >= s]

-- ex 11: Altere cada um dos algoritmos de ordenação do exercício 10 para também
-- cabilizar e retornar o número de comparações (do tipo x <= y)
-- a) bubble

bubble2 :: (Num b, Ord a) => [a] -> ([a], b)
bubble2 [] = ([], 0)
bubble2 lst = bolhaOrd2 (lst, 0) (length lst)

bolhaOrd2 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrd2 (lst, c) 0 = (lst, c)
bolhaOrd2 (lst, c) n = bolhaOrd2 (troca2 (lst, c)) (n -1)

troca2 :: (Ord a, Num b) => ([a], b) -> ([a], b)
troca2 ([x], c) = ([x], c)
troca2 ((x : y : zs), c) =
  if x > y
    then add (troca2 ((x : zs), c + 1)) y
    else add (troca2 ((y : zs), c + 1)) x
  where
    add (lst, c) a = (a : lst, c)

-- Selection
selection2 :: Ord a => [a] -> ([a], Int)
selection2 lst = auxiliar lst 0

auxiliar :: (Ord a) => [a] -> Int -> ([a], Int)
auxiliar [] n = ([], n)
auxiliar (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (auxiliar (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] c = (x, c)
minimo2 (x : y : xs) c
  | x > y = minimo2 (y : xs) (c + 1)
  | otherwise = minimo2 (x : xs) (c + 1)

--Insertion

insertion2 :: (Ord a) => [a] -> ([a], Int)
insertion2 [] = ([], 0)
insertion2 [x] = ([x], 0)
insertion2 (h : t) =
  let (sortedTail, n) = insertion2 t

      (lst, n1) = insereOrd2 h sortedTail n
   in (lst, n1)

insereOrd2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd2 x [] n = ([x], n)
insereOrd2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insereOrd2 x t (n + 1)) h
  where
    add (lst, n) y = (y : lst, n)

--Quick
auxiliarQuick :: [a] -> Int -> (a -> Bool) -> ([a], Int)
auxiliarQuick [] n _ = ([], n)
auxiliarQuick (x : xs) n cond =
  if (cond x)
    then add (auxiliarQuick xs (n + 1) cond) x
    else auxiliarQuick xs (n + 1) cond
  where
    add (lst, n) y = (y : lst, n)

quick2 :: (Ord a) => [a] -> ([a], Int)
quick2 [] = ([], 0)
quick2 (piv : xs) =
  let (left, nL) = auxiliarQuick xs 0 (<= piv)
      (right, nR) = auxiliarQuick xs 0 (> piv)
      (sortedL, nL1) = quick2 left
      (sortedR, nR1) = quick2 right
   in (sortedL ++ [piv] ++ sortedR, nL + nR + nL1 + nR1)

--ex 12: Altere cada um dos algoritmos de ordenação do exercício 10 para que façam
-- ordenação decrescente e também retornem o número de comparações. Teste
-- com as lsts dadas.

--Bubble
bubble3 :: (Ord a) => [a] -> ([a], Int)
bubble3 [] = ([], 0)
bubble3 lst = bolhaOrd3 (lst, 0) (length lst)

bolhaOrd3 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrd3 (lst, cont) 0 = (lst, cont)
bolhaOrd3 (lst, cont) n = bolhaOrd3 (troca3 (lst, cont)) (n -1)

troca3 :: (Ord a, Num b) => ([a], b) -> ([a], b)
troca3 ([x], c) = ([x], c)
troca3 ((x : y : zs), c) =
  if x > y
    then add (troca3 ((y : zs), c + 1)) x
    else add (troca3 ((x : zs), c + 1)) y
  where
    add (lst, cont) a = (a : lst, cont)

-- Selection

selection3 :: Ord a => [a] -> ([a], Int)
selection3 lst = auxiliar3 lst 0

auxiliar3 :: (Ord a) => [a] -> Int -> ([a], Int)
auxiliar3 [] n = ([], n)
auxiliar3 (x : xs) n =
  let (least, n_num) = minimo3 (x : xs) n

      remove3 _ [] = []
      remove3 n (h : t) =
        if (n == h)
          then t
          else h : (remove3 n t)

      add (lst, n) y = (y : lst, n)
   in add (auxiliar3 (remove3 least (x : xs)) n_num) least

minimo3 :: (Ord a) => [a] -> Int -> (a, Int)
minimo3 [] _ = undefined
minimo3 [x] c = (x, c)
minimo3 (x : y : xs) c
  | x > y = minimo3 (x : xs) (c + 1)
  | otherwise = minimo3 (y : xs) (c + 1)

--Insertion

insertion3 :: (Ord a) => [a] -> ([a], Int)
insertion3 [] = ([], 0)
insertion3 [x] = ([x], 0)
insertion3 (h : t) =
  let (sortedTail, n) = insertion3 t

      (lst, n1) = insereOrd3 h sortedTail n
   in (lst, n1)

insereOrd3 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd3 x [] n = ([x], n)
insereOrd3 x (h : t) n =
  if (x >= h)
    then ((x : h : t), n + 1)
    else add (insereOrd3 x t (n + 1)) h
  where
    add (lst, n) y = (y : lst, n)

--Quick
auxiliarQuick3 :: [a] -> Int -> (a -> Bool) -> ([a], Int)
auxiliarQuick3 [] n _ = ([], n)
auxiliarQuick3 (x : xs) n cond =
  if (cond x)
    then auxiliarQuick3 xs (n + 1) cond
    else add (auxiliarQuick3 xs (n + 1) cond) x
  where
    add (lst, n) y = (y : lst, n)

quick3 :: (Ord a) => [a] -> ([a], Int)
quick3 [] = ([], 0)
quick3 (piv : xs) =
  let (left, nL) = auxiliarQuick3 xs 0 (<= piv)
      (right, nR) = auxiliarQuick3 xs 0 (> piv)
      (sortedL, nL1) = quick3 left
      (sortedR, nR1) = quick3 right
   in (sortedL ++ [piv] ++ sortedR, nL + nR + nL1 + nR1)