module Lib where
import Text.Show.Functions



type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]


mayor :: Ord b=> (a->b)->a->a->Bool

mayor f v1 v2 = f v1 > f v2


menor:: Ord b=> (a->b)->a->a->Bool
menor f v1 v2 = f v1 < f v2

-- ordenarSegun (menor length) lista

--punto 2
ubicadoEn ::  [Barrio] -> Requisito
ubicadoEn barrios departamento = elem (barrio departamento) barrios


--cumpleRango que a partir de una función y dos números, indique si el valor retornado por 
--la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.

cumpleRango :: Depto-> (Depto->Float) -> Float -> Float -> Bool

cumpleRango depto f num1 num2 = between num1 num2 (f depto)

--Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.
cumpleBusqueda:: Busqueda -> Depto -> Bool

cumpleBusqueda listaRequisitos depto = (todosCumplen.aplicarRequisito depto) listaRequisitos

todosCumplen = all (==True)
aplicarRequisito depto = map ($ depto)

cumpleBusqueda' listaRequisitos depto = all (cumpleRequisito depto) listaRequisitos

cumpleRequisito depto requisito = requisito depto


type ListaDepartamentos = [Depto]
buscar :: Busqueda -> (Depto ->Depto-> Bool)-> ListaDepartamentos-> ListaDepartamentos

buscar busqueda critOrdenamiento listaDeptos = ordenarSegun critOrdenamiento.filter (cumpleBusqueda busqueda) $ listaDeptos

--Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas 
--retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.

mailsDePersonasInteresadas:: Depto -> [Persona] -> [Mail]

mailsDePersonasInteresadas depto listaPersonas = map mail.filter (estaInteresada depto) $ listaPersonas

estaInteresada :: Depto -> Persona -> Bool
estaInteresada depto persona = any (flip cumpleBusqueda depto).busquedas $ persona

