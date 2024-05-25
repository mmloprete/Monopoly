import Text.Show.Functions()

type Accion = Persona -> Persona

data Propiedad = UnaPropiedad {
    nombrePropiedad :: String,
    precioPropiedad :: Int
} deriving Show

data Persona = UnaPersona {
    nombre      ::  String,
    dinero      ::  Int,
    tactica     ::  String,
    propiedades :: [Propiedad],
    acciones    :: [Accion]
} deriving Show

carolina :: Persona
carolina = UnaPersona {
    nombre      =   "Carolina",
    dinero      =   500,
    tactica     =   "Accionista",
    propiedades =   [],
    acciones    =   [pasarPorElBanco, pagarAAccionistas]
}

manuel  ::  Persona
manuel = UnaPersona {
    nombre      =   "Manuel",
    dinero      =   500,
    tactica     =   "Oferente singular",
    propiedades = [],
    acciones    = [pasarPorElBanco, enojarse]
}

{--Auxiliares--}
--cambiarAlgo usan una función del valor que quiero cambiar y devuelven una persona con el valor cambiado
cambiarNombre :: (String -> String) -> Persona -> Persona
cambiarNombre fn persona = persona {nombre = fn . nombre $ persona}

cambiarDinero :: (Int -> Int) -> Persona -> Persona
cambiarDinero fn persona = persona {dinero = fn . dinero $ persona}

cambiarTactica :: (String -> String) -> Persona -> Persona
cambiarTactica fn persona = persona {tactica = fn . tactica $ persona}

-- (accion :) es una funcion aplicada parcialmente que añade la accion al inicio de la lista
-- (propiedad :) es una funcion aplicada parcialmente que añade la propiedad al inicio de la lista
agregarAccion :: Accion -> Persona -> Persona
agregarAccion accion persona = persona { acciones = (accion :).acciones $ persona}

agregarPropiedad :: Propiedad -> Persona -> Persona
agregarPropiedad propiedad persona = persona {propiedades = (propiedad :).propiedades $ persona}
-- filter filtra por una condición (filtra todas las propiedades distintas al nombre de la propiedad que le paso por parametro)
-- (/=) función que verifica que dos valores sean distintos
quitarPropiedad :: Propiedad -> Persona -> Persona
quitarPropiedad propiedad persona = persona {
    propiedades = filter((/=) (nombrePropiedad propiedad) . nombrePropiedad) . propiedades $ persona
}
{--Auxiliares--}

{--Acciones--}

--const toma el primer argumento e ignora el 2do que le llega (el primer elemento es "Comprador Compulsivo" y el segundo es la
--táctica actual de la persona

--pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”
pasarPorElBanco :: Accion
pasarPorElBanco persona = cambiarTactica (const "Comprador Compulsivo"). cambiarDinero (+40) $ persona

--enojarse: suma $50 y agrega gritar a sus acciones.
enojarse :: Accion
enojarse persona = agregarAccion gritar .cambiarDinero (+50) $ persona

--gritar: agrega “AHHHH” al principio de su nombre.
gritar :: Accion
gritar persona = cambiarNombre ("AHHHH" ++) persona

--subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente
--singular” o “Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la
--propiedad de su dinero y sumar la nueva adquisición a sus propiedades.

esTacticaGanadora :: String -> Bool
esTacticaGanadora "Oferente Singular"   = True
esTacticaGanadora "Accionista"          = True
esTacticaGanadora _                     = False

adquirirPropiedad :: Propiedad -> Persona -> Persona
adquirirPropiedad propiedad persona =
    cambiarDinero (subtract . precioPropiedad $ propiedad) . agregarPropiedad propiedad $ persona

subastar :: Propiedad -> Accion
subastar propiedad persona
    | esTacticaGanadora . tactica $ persona = adquirirPropiedad propiedad persona
    | otherwise                             = persona

--cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara
--obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = precioPropiedad propiedad < 150

cobrarAlquileresBaratos :: Persona -> Int
cobrarAlquileresBaratos persona = length . filter esPropiedadBarata . propiedades $ persona * 10

cobrarAlquileresCaros :: Persona -> Int
cobrarAlquileresCaros persona = length . filter (not . esPropiedadBarata) . propiedades $ persona * 20

cobrarAlquileres :: Accion
cobrarAlquileres persona = persona {
    dinero = dinero persona + cobrarAlquileresCaros persona + cobrarAlquileresBaratos persona
}

--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea
-- “Accionista”, en ese caso suma $200.

esTacticaAccionista :: String -> Bool
esTacticaAccionista "Accionista" = True
esTacticaAccionista _            = False

pagarAAccionistas :: Accion
pagarAAccionistas persona
    | esTacticaAccionista . tactica $ persona = cambiarDinero (+200) persona
    | otherwise                               = cambiarDinero (subtract 100) persona
