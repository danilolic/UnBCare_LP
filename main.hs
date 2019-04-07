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

-- tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)

cadastrarAlarmes :: PlanoMedicamento -> Horario
cadastrarAlarmes planoMedicamento =
  sort (concat (map (\x -> fsnd x ) planoMedicamento))

-- HELPERS

ffst :: (a, b, c) -> a
ffst (a, b, c) = a

fsnd :: (a, b, c) -> b
fsnd (a, b, c) = b

ftrd :: (a, b, c) -> c
ftrd (a, b, c) = c


-- let medicamentos = [("r1",10), ("r2",7),("r3",8)]
-- let planoMedicamento = [("r1",[5,8,9],10), ("r2",[8,5,4],7),("r3",[9,5,1],8)]