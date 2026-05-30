-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | TenTimes C   ---- Executa o comando C 10 vezes
    | Repeat C B --- Repeat C until B: executa C enquanto B é falso
    | Loop E E C      ---- Loop e1 e2 c: executa (e2 - e1) vezes o comando C 
    | DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y 
   deriving(Eq,Show)                


-----------------------------------------------------
----- As próximas funções servem para manipular a memória (sigma)
-----------------------------------------------------

type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

procuraVar :: Memoria -> String -> Int
procuraVar [] v = error ("Variavel " ++ v ++ " nao definida no estado")
procuraVar ((s,i):xs) v 
  | s == v    = i
  | otherwise = procuraVar xs v

-- Substituído 'any' e 'map' por recursão pura estrutural
mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v    = (s,n) : xs
  | otherwise = (s,i) : mudaVar xs v n


-------------------------------------
--- semântica big-step para expressões aritméticas (expressions)
-------------------------------------

ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s)       = procuraVar s x
ebigStep (Num n,s)       = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)   = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2,s)   = ebigStep (e1,s) `div` ebigStep (e2,s)


-------------------------------------
--- semântica big-step para expressões booleanas (booleans)
-------------------------------------

bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)        = True
bbigStep (FALSE,s)       = False
bbigStep (Not b,s)       = not (bbigStep (b,s))
bbigStep (And b1 b2,s)   = bbigStep (b1,s) && bbigStep (b2,s)
bbigStep (Or b1 b2,s)    = bbigStep (b1,s) || bbigStep (b2,s)
bbigStep (Leq e1 e2,s)   = ebigStep (e1,s) <= ebigStep (e2,s)
bbigStep (Igual e1 e2,s) = ebigStep (e1,s) == ebigStep (e2,s)


-------------------------------------
--- semântica big-step para comandos imperativos (commands)
-------------------------------------

cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)

cbigStep (Atrib (Var x) e, s) = (Skip, mudaVar s x (ebigStep (e,s)))

cbigStep (Seq c1 c2, s) = 
  let (_, s') = cbigStep (c1, s)
  in cbigStep (c2, s')

cbigStep (If b c1 c2, s) = 
  if bbigStep (b, s) then cbigStep (c1, s) else cbigStep (c2, s)

cbigStep (While b c, s) = 
  if bbigStep (b, s) 
  then let (_, s') = cbigStep (c, s) in cbigStep (While b c, s')
  else (Skip, s)

-- TenTimes C: Reescrevido para usar a função auxiliar executaVezes nativa da semântica
cbigStep (TenTimes c, s) = (Skip, executaVezes 10 c s)

-- Repeat C Until B: Mantido com a lógica pura e intermediária correta
cbigStep (Repeat c b, s) = 
  let (_, s') = cbigStep (c, s)
  in if bbigStep (b, s') then (Skip, s') else cbigStep (Repeat c b, s')

-- Loop E1 E2 C: Reescrevido para usar a função auxiliar executaVezes
cbigStep (Loop e1 e2 c, s) = 
  let vezes = ebigStep (e2, s) - ebigStep (e1, s)
  in if vezes <= 0 
     then (Skip, s)
     else (Skip, executaVezes vezes c s)

-- DuplaATrib: Avalia ambos na foto antiga e aplica sequencialmente as mudanças
cbigStep (DuplaATrib (Var v1) (Var v2) e1 e2, s) =
  let val1 = ebigStep (e1, s)
      val2 = ebigStep (e2, s)
  in (Skip, mudaVar (mudaVar s v1 val1) v2 val2)

-- AtribCond: Avaliação condicional atômica
cbigStep (AtribCond b (Var v1) e1 e2, s) =
  if bbigStep (b, s)
  then (Skip, mudaVar s v1 (ebigStep (e1, s)))
  else (Skip, mudaVar s v1 (ebigStep (e2, s)))

-- Swap: Troca usando o estado s imutável
cbigStep (Swap (Var x) (Var y), s) =
  let valX = procuraVar s x
      valY = procuraVar s y
  in (Skip, mudaVar (mudaVar s x valY) y valX)

-- Executa um comando C n vezes em sequência na memória
executaVezes :: Int -> C -> Memoria -> Memoria
executaVezes 0 _ s = s
executaVezes n c s = 
  let (_, s') = cbigStep (c, s)
  in executaVezes (n - 1) c s'


--------------------------------------
--- exemplos de programas para teste
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

memoriaTeste :: Memoria
memoriaTeste = [("x", 5), ("y", 10), ("z", 0), ("w", 2)]

exLoop :: C
exLoop = Loop (Num 2) (Num 5) (Atrib (Var "z") (Soma (Var "z") (Num 2)))

exDuplaAtrib :: C
exDuplaAtrib = DuplaATrib (Var "x") (Var "y") (Var "y") (Num 20)

exRepeat :: C
exRepeat = Repeat (Atrib (Var "z") (Soma (Var "z") (Num 1))) (Igual (Var "z") (Num 3))

exSwap :: C
exSwap = Swap (Var "x") (Var "y")

exAtribCond :: C
exAtribCond = AtribCond (Leq (Var "x") (Num 5)) (Var "z") (Num 100) (Num 999)

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))