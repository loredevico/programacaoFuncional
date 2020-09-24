--Exercício 1 Refaça as seguintes funções dos roteiros 2, 3 e 4, 
-- utilizando o comando “where”

--  a) Escreva a função valida que indica se uma data é válida ou não.

bissextoWhere :: Int -> Bool
bissextoWhere a = exp_1 || (exp_2 && exp_3)
  where
    exp_1 = (mod a 400 == 0)
    exp_2 = (mod a 4 == 0)
    exp_3 = (mod a 100 /= 0)

type Data = (Int, Int, Int)

validaWhere :: Data -> Bool
validaWhere (d, m, a) = exp_1 || exp_2 || exp_3 || exp_4
  where
    exp_1 = d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
    exp_2 = d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11)
    exp_3 = d >= 1 && d <= 28 && m == 2 && not (bissextoWhere a)
    exp_4 = d >= 1 && d <= 29 && m == 2 && (bissextoWhere a)

-- b) Escreva a função bissextos a seguir que recebe uma lista de inteiros e
-- retorna uma lista com os valores que representam as bissexto.

    bissextosWhere :: [Int] -> [Int]
    bissextosWhere lst = exp_1
        where
            exp_1 = [ x | x <- lst, bissextoWhere x ]

-- c) Escreva a função atrasados que recebe um parâmetro do tipo
-- Emprestimos e a Data atual, e retorna uma lista com todos os
-- empréstimos atrasados.

type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimoWhere :: Emprestimos
bdEmprestimoWhere =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

validacaoWhere :: Data -> Data -> Bool
validacaoWhere (d, m, a) (d2, m2, a2) = not (exp_1 || exp_2 || exp_3 || exp_4)
  where
    exp_1 = not (validaWhere (d, m, a)) || not (validaWhere (d2, m2, a2))
    exp_2 = a > a2
    exp_3 = a == a2 && m > m2
    exp_4 = a == a2 && m == m && d > d2

emd :: Data -> Emprestimo -> Bool
emd dataAtual (codLivro, codAluno, dataEmprest, dataDevolucaolucao, status) = exp_1
  where
    exp_1 = validacaoWhere dataAtual dataDevolucaolucao

atrasadosWhere :: Emprestimos -> Data -> Emprestimos
atrasadosWhere lstEmprestimos dataAtual = exp_1
  where
    exp_1 = [x | x <- lstEmprestimos, not (emd dataAtual x)]

-- d) Faça uma segunda definição da função recursiva fibo2 que retorna
-- o n-ésimo termo da sequência de Fibonacci utilizando recursividade
-- e os conceitos a seguir (use a função caminho(x,y)).

caminhoWhere :: (Int, Int) -> (Int, Int)
caminhoWhere (a, b) = exp_1
  where
    exp_1 = (b, a + b)

fibo2Where :: Int -> (Int, Int)
fibo2Where 0 = (0, 1)
fibo2Where x = exp_1
  where
    exp_1 = caminhoWhere (fibo2Where (x -1))

-- e) Escreva a função fatorial usando a função prodIntervalo.

prodIntervaloWhere :: Int -> Int -> Int
prodIntervaloWhere a b = if (a >= b) then exp_1 else exp_2
  where
    exp_1 = b
    exp_2 = (a * (prodIntervaloWhere (a + 1) b))

fatorialInter :: Int -> Int
fatorialInter x = exp_1
  where
    exp_1 = prodIntervaloWhere 1 x

-- 2) Refaça as funções do exercício 1, utilizando o comando “let” para
-- definições locais (incluindo funções auxiliares que são necessárias na
-- solução da função principal). Repetir para os itens “a” a “e”. 

-- a) 

bissextoLet :: Int -> Bool
bissextoLet ano =
  let exp_1 = (mod ano 400 == 0)
      exp_2 = (mod ano 4 == 0)
      exp_3 = (mod ano 100 /= 0)
   in exp_1 || (exp_2 && exp_3)

type DataLet = (Int, Int, Int)

validaLet :: DataLet -> Bool
validaLet (d, m, a) =
  let exp_1 = d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
      exp_2 = d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11)
      exp_3 = d >= 1 && d <= 28 && m == 2 && not (bissextoLet a)
      exp_4 = d >= 1 && d <= 29 && m == 2 && (bissextoLet a)
   in exp_1 || exp_2 || exp_3 || exp_4

--B)
bissextosLet :: [Int] -> [Int]
bissextosLet lst =
  let exp_1 = [ x | x <- lst, bissextoLet x ]
   in exp_1

--C)
type EmprestimoLet = (String, String, DataLet, DataLet, String)

type EmprestimosLet = [EmprestimoLet]

bdEmprestimoLet :: EmprestimosLet
bdEmprestimoLet =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

validacaoLet :: DataLet -> DataLet -> Bool
validacaoLet (d, m, a) (d2, m2, a2) =
  let exp_1 = not (validaLet (d, m, a)) || not (validaLet (d2, m2, a2))
      exp_2 = a > a2
      exp_3 = a == a2 && m > m2
      exp_4 = a == a2 && m == m && d > d2
   in not (exp_1 || exp_2 || exp_3 || exp_4)

emDiaLet :: DataLet -> EmprestimoLet -> Bool
emDiaLet dataAtual (codLivro, codAluno, dataEmprest, dataDevolucao, status) =
  let exp_1 = validacaoLet dataAtual dataDevolucao
   in exp_1

atrasadosLet :: EmprestimosLet -> DataLet -> EmprestimosLet
atrasadosLet lstEmprestimos dataAtual =
  let exp_1 = [ x | x <- lstEmprestimos, not (emDiaLet dataAtual x)]
   in exp_1

--D)
caminhoLet :: (Int, Int) -> (Int, Int)
caminhoLet (a, b) =
  let exp_1 = (b, a + b)
   in exp_1

fibo2Let :: Int -> (Int, Int)
fibo2Let 0 = (0, 1)
fibo2Let x =
  let exp_1 = caminhoLet (fibo2Let (x - 1))
   in exp_1

--E)
prodIntervaloLet :: Int -> Int -> Int
prodIntervaloLet a b =
  let exp_1 =
        if (a >= b)
          then b
          else (a * (prodIntervaloLet (a + 1) b))
   in exp_1

fatInterLet :: Int -> Int
fatInterLet x =
  let exp_1 = prodIntervaloLet 1 x
   in exp_1

-- 3) Aplicar Beta-redução nas exp_ressões lambda a seguir: 
-- a) (λ x.2*x + 1) 3

-- b) (λ xy.x-y) 5 7

-- c) (λ yx.x-y) 5 7

-- d) (λ xy.x-y) (λz.z/2)

-- e) (λ xy.x-y) ((λz.z/2)6)1

-- f) (λ x.λ y. – x y) 9 4

-- g) (λ x .xx) (λ y .y)

-- 4) Está como PNG. Avaliação de expressões.

-- 5) Codifique as seguintes expressões do cálculo lambda em Haskell e avalie as
-- mmas no GHCi:
    -- a) (λx λy. y)((λz. z)(λz. z))(λw. w) 5
    -- (\x -> \y -> y) ((\z -> z) (\z -> z)) (\w -> w) 5

    -- b) ((λf. (λx. f(f x))) (λy. (y * y))) 3
    -- ((\f -> (\x -> f(f x))) (\y -> (y * y))) 3

    -- c) ((λf. (λx. f(f x)))(λy.(+ y y))) 5
    -- ((\f -> (\x -> f(f x))) (\y -> (+ y y))) 5
    
    -- d) ((λx. (λy. + x y) 5) ((λy. - y 3) 7))
    -- ((\x -> (\y -> + x y) 5) (( \y -> - y 3) 7))

    -- e) (((λf. (λx. f(f(f x)))) (λy. (y * y))) 2)
    -- (((\f -> (\x -> f(f(f x)))) (\y -> (y * y))) 2)

    -- f) (λx. λy. + x ((λx. - x 3) y)) 5 6
    -- (\x -> \y -> + x ((\x -> - x 3) y)) 5 6

