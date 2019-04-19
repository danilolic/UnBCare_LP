import Data.List

type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]
type Preco = Int
type Farmacia = (Nome,[(Medicamento,Preco)])
type Mercado = [Farmacia]
type Compra = (Preco, Nome)

adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento medicamento medicamentos =
  medicamento:medicamentos

removerMedicamento :: Nome -> Medicamentos -> Medicamentos
removerMedicamento nome medicamentos =
  filter (\x -> fst(x) /= nome) medicamentos

consultarMedicamento :: Nome -> Medicamentos -> Medicamento
consultarMedicamento nome medicamentos
  | filter (\x -> fst(x) == nome) medicamentos == [] = ("", 0)
  | otherwise = head(filter (\x -> fst(x) == nome) medicamentos)

alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
alterarMedicamento _ [] = []
alterarMedicamento medicamento (x:xs)
  | fst x == fst medicamento = (fst x, snd medicamento):xs
  | otherwise = x:alterarMedicamento medicamento xs

tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos
tomarMedicamentoSOS _ [] = []
tomarMedicamentoSOS nome (x:xs)
  | fst x == nome = (nome, snd(x)-1):xs
  | otherwise = x:tomarMedicamentoSOS nome xs


{-
**QUESTÃO 6, valor 1,0 ponto
Defina a função tomarMedicamentosHorario que, a partir  de um plano de medicamentos, de uma lista de
medicamentos, e da hora atual, retorna um par com o plano e a lista de medicamentos atualizados.
O comportamento da função deve ser tal que, no horário atual, o paciente tome todos os remédios
eventualmente previstos para aquele horário. Isso resulta em que a quantidade desses medicamentos seja
atualizada e em que o plano seja atualizado no sentido em que tais remedios sejam tomados no próximo
horário previsto. O tipo da função tomarMedicamentosHorario é o seguinte:

tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)
-}
-- tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)
-- tomarMedicamentosHorario (x:xs) _ _ = []
-- tomarMedicamentosHorario (x:xs) medicamentos horaAtual
--   | nextElem(fsnd x horaAtual) /= -1 = ((ffst x, fsnd x, nextElem fsnd x horaAtual), medicamentos)
--   | otherwise = tomarMedicamentosHorario xs medicamentos horaAtual

cadastrarAlarmes :: PlanoMedicamento -> Horario
cadastrarAlarmes planoMedicamento =
  sort (concat (map (\x -> fsnd x ) planoMedicamento))


listarMedicamentosComprar :: Medicamentos ->  Medicamentos
listarMedicamentosComprar medicamentos =
  filter (\x -> snd(x) == 0) medicamentos

comprarMedicamentosDias ::  PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
comprarMedicamentosDias [] [] _ = []
comprarMedicamentosDias (x:xs) (y:ys) dias
  | length(fsnd x) * dias > snd y = (fst y, length(fsnd x) * dias - snd y):comprarMedicamentosDias xs ys dias
  | otherwise = (fst y, 0):comprarMedicamentosDias xs ys dias

{-
**QUESTÃO 10, valor 1,5 ponto
Defina a função comprarMedicamentosPreco que, a partir de uma lista de medicamentos e de um mercado
que consiste de uma lista de farmacias, retorne uma compra informando o nome da farmácia e o valor a
ser gasto nela com a compra dos medicamentos. Essa farmácia deve ser tal que tenha todos os medicamentos
na quantidade necessária para cada medicamento e que tenha o menor preço para a compra deles.
O tipo da função comprarMedicamentosPreco é o seguinte:

comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra

**Dica: determine as farmacias que tem todos os medicamentos requeridos na quantidade necessária;
orce os medicamentos em cada uma dessas farmácias e, dentre estas, escolha a que oferece o menor
preço pelo total dos medicamentos necessarios. Se existir mais de uma farmácia que ofereça esse
mesmo preço total, escolha qualquer uma.
-}

-- HELPERS

ffst :: (a, b, c) -> a
ffst (a, b, c) = a

fsnd :: (a, b, c) -> b
fsnd (a, b, c) = b

ftrd :: (a, b, c) -> c
ftrd (a, b, c) = c

nextElem :: [Int] -> Int -> Int
nextElem [] _ = -1
nextElem (x:xs) n
  | x == n = head xs
  | otherwise = nextElem xs n

-- medicamentos = [("r1",10), ("r2",7),("r3",8)]
-- planoMedicamento = [("r1",[5,8,9],10), ("r2",[8,5,4],7),("r3",[9,5,1],8)]
-- mercado = [("F1",[(("R1",100),20),(("R2",100),5),(("R3",100),600)]),
--            ("F2",[(("R1",100),10),(("R2",100),5),(("R3",100),600)]),
--            ("F3",[(("R1",100),15),(("R2",100),2),(("R3",100),100)])]