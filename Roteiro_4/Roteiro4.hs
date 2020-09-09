--Loredana Romano Devico

-- 1) Avalie os seguintes exemplos.

lst1:: [Integer]
lst1 = [ x * 2 | x <- [1 .. 10], x * 2 >= 12]

lst2:: [Integer]
lst2 = [ x | x <- [50 .. 100], mod x 7 == 3]

lst3:: [Integer]
lst3 = [ x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]

lst4:: [(Integer, Integer)]
lst4 = [(x, y) | x <- [1 .. 4], y <- [x .. 5]]

-- 2)  Escreva a função quadrados que recebe dois inteiros e retorna os quadrados dos
-- números entre eles.

quadrados :: Integer -> Integer -> [Integer]
quadrados prim ult = [ x ^ 2 | x <- [prim .. ult] ]

-- 3) Usando lista por compreensão, escreva a função seleciona_ímpares que recebe um
-- lista de inteiros e retorna uma nova lista com todos os números ímpares presentes na lista
-- de entrada.

seleciona_impares :: [Integer] -> [Integer]
seleciona_impares lst = [ x | x <- lst, odd x ]

-- 4) Escreva a função tabuada que recebe um valor inteiro e retorna a lista de seus dez
-- primeiros múltiplos.

tabuada :: Integer -> [Integer]
tabuada num = [num * mult | mult <- [1 .. 10]]

-- 5) Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma
-- lista com os valores que representam anos bissextos.

bissexto :: Integer -> Bool
bissexto ano
  | (mod ano 400 == 0) = True
  | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
  | otherwise = False
bissextolista :: [Integer] -> [Integer]
bissextolista anos = [x | x <- anos, bissexto x]

-- 6) Usando lista por compreensão, escreva a função sublistas que recebe uma lista
-- formada por sublistas de um mesmo tipo e retorna uma lista com todos os elementos da
-- lista de entrada na mesma ordem.

sublista :: [[Integer]] -> [Integer]
sublista lista = [ x | item <- lista, x <- item]

-- 7) Sejam os tipos Data, Emprestimo, Emprestimos e a variável bdEmprestimo do exemplo
-- da Biblioteca. Escreva a função atrasados que recebe um parâmetro do tipo Emprestimos
-- e a Data atual, e retorna uma lista com todos os empréstimos atrasados. 

type Data = (Int, Int, Integer)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

-- Verificar se a data colocada é válida// semelhante a exercício nos slides: 

validacao :: Data -> Bool
validacao (dia, mes, ano)
  | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
  | dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = True
  | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True
  | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True
  | otherwise = False

emDia :: Data -> Data -> Bool
emDia (dia, mes, ano) (dia2, mes2, ano2)
  | not (validacao (dia, mes, ano)) || not (validacao (dia2, mes2, ano2)) = False
  | ano > ano2 = False
  | ano == ano2 && mes > mes2 = False
  | ano == ano2 && mes == mes && dia > dia2 = False
  | otherwise = True

emprestimoValido :: Data -> Emprestimo -> Bool
emprestimoValido dataAtual (codLivro, codAluno, dataEmprest, dataDevolucao, status) =
  emDia dataAtual dataDevolucao

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaDeEmprestimos dataAtual = [x | x <- listaDeEmprestimos, not (emprestimoValido dataAtual x)]

--8) Escreva a função recursiva npares que recebe uma lista de inteiros e retorna a
-- quantidade de números pares pertencentes à lista.

npares :: [Int] -> Int
npares listaInteiros = length [ x | x <- listaInteiros, even x]

-- 9) Escreva a função recursiva produtorio que recebe uma lista de números e retorna o
-- produto de todos os seus elementos.

produtorio::[Integer] -> Integer
produtorio [] = 1
produtorio (head : tail) = head * produtorio tail

-- 10)  Escreva a função recursiva comprime a seguir que recebe uma lista de listas e retorna
-- uma lista contendo todos os elementos das sublistas.

comprime :: [[x]] -> [x]
comprime [[]] = []
comprime ([]:tail) = comprime tail
comprime ((head:head1):tail) = head:(comprime (head1:tail))

-- 11)  Escreva a função tamanho a seguir que recebe uma lista polimórfica (de qualquer
-- tipo) e retorna a quantidade de elementos que ela possui.

tamanho :: [a] -> Integer
tamanho [] = 0
tamanho (head : tail) = 1 + tamanho tail

-- 12)  (Compreensão) escreva a função uniaoNRec a seguir que faz a união de duas listas de modo que 
-- ela mantenha todos os elementos da 1a lista na mesma ordem

membro :: Eq t => t -> [t] -> Bool
membro a [] = False
membro a (head : tail)
  | a == head = True
  | otherwise = membro a tail

uniaoNRec :: [Integer] -> [Integer] -> [Integer]
uniaoNRec l1 l2 = [x | x <- l1] ++ [y | y <- l2, not (membro y l1)]

-- 13) (Recursão) Escreva a função uniaoRec2 a seguir que faz a união de duas listas de
-- modo que ela mantenha todos os elementos da 1a lista na mesma ordem

uniaoRec2 :: [Integer] -> [Integer] -> [Integer]
uniaoRec2 (a : ax) (b : bx)
  | (a == b) = uniaoRec2 ax bx
  | otherwise = a : ax ++ b : bx