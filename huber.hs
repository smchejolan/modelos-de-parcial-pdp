--Modelo de parcial 31/5/17
import Text.Show.Functions

data Chofer = Chofer{
    nombreChofer :: String,
    kilometraje :: Float,
    viajes :: [Viaje],
    condicion :: Condicion  
} deriving (Show)

data Viaje = Viaje{
    fecha :: (Int,Int,Int),
    cliente :: Cliente,
    precio :: Float
} deriving (Show)

data Cliente = Cliente{
    nombreCliente :: String, 
    viveEn :: String
} deriving (Show)

type Condicion = Viaje -> Bool

cualquierViaje :: Condicion 
cualquierViaje _ = True

soloViajesDeMasDe200 :: Condicion
soloViajesDeMasDe200 = (<200) . precio

condicionSegunNombreDeCliente :: Int -> Condicion
condicionSegunNombreDeCliente letras = (<letras) . length . nombreCliente . cliente

noVaAXZona :: String -> Condicion 
noVaAXZona zona = (/=zona) . viveEn . cliente

lucas :: Cliente 
lucas = Cliente {
    nombreCliente = "Lucas",
    viveEn = "Victoria"
}

daniel :: Chofer
daniel = Chofer {
    nombreChofer = "Daniel",
    kilometraje = 23500,
    viajes = [viajeDeLucas],
    condicion = noVaAXZona "Olivos"
}

viajeDeLucas :: Viaje 
viajeDeLucas = Viaje{
    fecha = (20,4,2017),
    cliente = lucas,
    precio = 150
} 

alejandra :: Chofer
alejandra = Chofer {
    nombreChofer = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condicion = cualquierViaje
}

puedeRealizarViaje :: Viaje -> Chofer -> Bool
puedeRealizarViaje viaje chofer = condicion chofer $ viaje

liquidacionChofer :: Chofer -> Float
liquidacionChofer = sum . map precio . viajes

realizarUnViaje :: Viaje -> [Chofer] -> Chofer 
realizarUnViaje viaje = (hacerViaje viaje) . elegirChofer . (filtrarChoferes viaje)

filtrarChoferes :: Viaje -> [Chofer] -> [Chofer]
filtrarChoferes viaje = filter (puedeRealizarViaje viaje) 

elegirChofer :: [Chofer] -> Chofer
elegirChofer = foldl1 menosViaje

menosViaje :: Chofer -> Chofer -> Chofer
menosViaje chofer1 chofer2  | cantViajesDelChofer chofer1 <= cantViajesDelChofer chofer2 = chofer1
                            | otherwise = chofer2

cantViajesDelChofer :: Chofer -> Int
cantViajesDelChofer = length . viajes

hacerViaje :: Viaje -> Chofer -> Chofer 
hacerViaje viaje chofer = chofer{
    viajes = viaje : viajes chofer
}  

nitoInfy :: Chofer 
nitoInfy = Chofer{
    nombreChofer ="Nito Infy",
    kilometraje = 70000,
    viajes = repetirViaje viajeDeLucas,
    condicion = condicionSegunNombreDeCliente 3
}

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

-- No se puede porque no se puede realizar la funcion liquidacionChofer sobre una lista infinita porque se deben pasar por todos los elementos de la lista que en este caso es infinita
-- Si se puede ya que la funcion puedeRealizarViaje solo recibe un viaje y usa la condicion del chofer, no usa en ningun momento la lista infinita, de hecho puedeRealizarViaje es True para el nuevoViaje

gongNeng :: Ord c => c -> (c -> Bool) -> (b -> c) -> [b] -> c
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3

