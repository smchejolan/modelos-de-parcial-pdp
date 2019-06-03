--Modelo de parcial 23/5/18
import Text.Show.Functions

type Deuda = Float 
type Poblacion = Int
type RecursoNatural = String
type Estrategia = Pais -> Pais
type DineroMDolares = Float
type Receta = [Estrategia]

data Pais = Pais {
    ingresoPerCapita :: Float,
    poblacionActivaSectorPublico :: Poblacion,
    poblacionActivaSectorPrivado :: Poblacion, 
    recursosNaturales :: [RecursoNatural], 
    deudaAlFmi :: DineroMDolares
} deriving (Show)

prestamo :: DineroMDolares -> Estrategia
prestamo prestamoEnMDollares pais = pais{
    deudaAlFmi = deudaAlFmi pais + cobrarInteres prestamoEnMDollares 
}  

cobrarInteres :: DineroMDolares -> DineroMDolares
cobrarInteres = (*1.5)

reducirPuestosSecorPublico :: Int -> Estrategia
reducirPuestosSecorPublico cantPuestosDeTrabajo pais = pais{
    poblacionActivaSectorPublico = poblacionActivaSectorPublico pais - cantPuestosDeTrabajo,
    ingresoPerCapita = ingresoPerCapita pais * (1 - reduccionDeIngresos cantPuestosDeTrabajo)
} 

reduccionDeIngresos :: Int -> Float
reduccionDeIngresos cantPuestosDeTrabajo | cantPuestosDeTrabajo > 100 = 0.2
                                         | otherwise = 0.15
                                        
darRecursoNaturalAEmpresa :: RecursoNatural -> Estrategia 
darRecursoNaturalAEmpresa recurso pais = pais {
    recursosNaturales = quitarRecurso recurso (recursosNaturales pais),
    deudaAlFmi = deudaAlFmi pais - 2
}

quitarRecurso :: RecursoNatural -> [RecursoNatural] -> [RecursoNatural]
quitarRecurso recurso recursosNaturalesPais = filter (/= recurso) recursosNaturalesPais

blindaje :: Estrategia 
blindaje pais = (prestamo (pbi pais * 0.5) . reducirPuestosSecorPublico 500) pais

pbi :: Pais -> DineroMDolares 
pbi pais = ingresoPerCapita pais * fromIntegral (poblacionActiva pais)

poblacionActiva :: Pais -> Poblacion 
poblacionActiva pais = poblacionActivaSectorPrivado pais + poblacionActivaSectorPublico pais

namibia :: Pais 
namibia = Pais{
    ingresoPerCapita = 4140,
    poblacionActivaSectorPublico = 400000,
    poblacionActivaSectorPrivado = 650000,
    recursosNaturales = ["Mineria", "ecoturismo"],
    deudaAlFmi = 50
}

----3

nuevaReceta :: Receta 
nuevaReceta = [prestamo 200, darRecursoNaturalAEmpresa "Mineria"]

receta2 :: Receta
receta2 = [prestamo 500, reducirPuestosSecorPublico 100]

receta3 :: Receta 
receta3 = [darRecursoNaturalAEmpresa "ecoturismo", prestamo 2000, blindaje]

--aplicarEstrategia :: Pais -> Estrategia -> Pais
--aplicarEstrategia pais estrategia = estrategia pais--  se puede hacer usando funcion anonima/expresion lamda y sirve para foldl 
-- $ :: (a->b) -> a -> b lo contrario a aplicarEstrategia y sirve para foldr, en este caso se puede usar cualquiera porque no importa el orden en que son aplicadas las estrategias

efectosColateralesReceta :: Receta -> Pais -> Pais 
efectosColateralesReceta receta pais = foldr ($) pais receta

---4

safan :: [Pais] -> [Pais]
safan = filter (elem "Petroleo" . recursosNaturales) 
-- elem "Petroleo" es aplicacion parcial
-- se usa la funcion que se le pasa de argumento a filter es composicion
-- recursosNaturales es orden superior
totalDeudaFmi :: [Pais] -> DineroMDolares 
totalDeudaFmi = sum . (map deudaAlFmi)
-- composicion en la funcion
-- deudaAlFmi es orden superior
-- y map deudaAlFmi es aplicacion parcial

----5

recetasOrdenadaDePeorAMejor :: [Receta] -> Pais -> Bool
recetasOrdenadaDePeorAMejor [receta] _ = True
recetasOrdenadaDePeorAMejor (receta1:receta2:recetas) pais = (pbiMejora receta1 receta2 pais) && recetasOrdenadaDePeorAMejor (receta2:recetas) pais  

pbiMejora :: Receta -> Receta -> Pais -> Bool
pbiMejora receta1 receta2 pais = (pbi . efectosColateralesReceta receta1) pais <= (pbi . efectosColateralesReceta receta2) pais

---6

