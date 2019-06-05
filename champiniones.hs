--Modelo de parcial 18/5/16
import Text.Show.Functions
import Data.List 
type Defensa = Float
type Ataque = Float
type Buff = Personaje -> Personaje

data Arma = Baculo{
    nombreBaculo :: String, inteligenciaBaculo :: Float
} | Espada {
        almasCosechadas :: Float, material :: String
} | Arco {
    longitud :: Float, danioBase :: Float, rango :: Float
} deriving (Show) 

data PartesDeArmadura = ParteDeArmadura{
    defensa :: Float,
    durabilidad :: Float
}  deriving (Show)

data Personaje = Personaje {
    salud :: Float,
    armadura :: [PartesDeArmadura],
    arma :: Arma,
    poderDefensa :: Defensa,
    poderAtaque :: Ataque
} deriving (Show)

poderDeDefensa :: Personaje -> Defensa
poderDeDefensa personaje = salud personaje + sumarNoArmadurasNoRotas personaje

sumarNoArmadurasNoRotas :: Personaje -> Defensa
sumarNoArmadurasNoRotas = sum . map (defensa) . filter ((>0) . durabilidad) . armadura

poderDeAtaque :: Arma -> Ataque 
poderDeAtaque Baculo{} = inteligenciaBaculo Baculo{} + genericLength (nombreBaculo Baculo{})
poderDeAtaque Espada{} = almasCosechadas Espada{} * coeficienteMaterial (material Espada{})  
poderDeAtaque Arco{} =  rango Arco{} * longitud Arco{} + danioBase Arco{}

coeficienteMaterial :: String -> Float
coeficienteMaterial "Madera" = 2
coeficienteMaterial "Metal" = 3
coeficienteMaterial _ = 1 

frenesi :: Buff
frenesi = modificarAtributos . mejorarArmadura 

mejorarArmadura :: Personaje -> Personaje 
mejorarArmadura personaje = personaje{
    armadura = map (modificarDefensaArmaduraSegun (\x -> x + x*0.2)) (armadura personaje)
} 

modificarAtributos :: Personaje -> Personaje 
modificarAtributos personaje = personaje{
    poderDefensa = poderDeDefensa personaje,
    poderAtaque = (poderDeAtaque . arma) personaje
}

mantoEtereo :: Buff
mantoEtereo = modificarAtributos . mejorarArmaduraEn3YBajarSalud

mejorarArmaduraEn3YBajarSalud :: Personaje -> Personaje
mejorarArmaduraEn3YBajarSalud personaje = personaje {
    salud = salud personaje - 100,
    armadura = map (modificarDefensaArmaduraSegun (+3)) (armadura personaje)
}

modificarDefensaArmaduraSegun :: (Float -> Float) -> PartesDeArmadura -> PartesDeArmadura
modificarDefensaArmaduraSegun modificador armadura = armadura {
    defensa = (modificador . defensa) armadura  
}

berseker :: Buff
berseker = modificarAtributos  . defensaEn2YArmaMetal

defensaEn2YArmaMetal :: Personaje -> Personaje
defensaEn2YArmaMetal personaje = personaje {
    armadura = map (modificarDefensaArmaduraSegun (\x->2)) (armadura personaje),
    arma = (mejorarMaterialArma .arma) personaje
}

mejorarMaterialArma :: Arma -> Arma 
mejorarMaterialArma Espada{}  | material Espada{} == "Madera" = Espada {material = "Metal"} 
                            | otherwise = Espada{}
mejorarMaterialArma Arco{} = Arco{}
mejorarMaterialArma Baculo{} = Baculo{} 

espejoDeKarma :: Buff -> Buff
espejoDeKarma buf = buf.buf 

sucesionDeBuffInesperados :: [Buff] -> Buff
sucesionDeBuffInesperados buffs personaje = foldr ($) personaje buffs

{-buffCreativo :: Buff 
buffCreativo = modificarAtributos . romperArmadura . cosechar100Almas
-}

