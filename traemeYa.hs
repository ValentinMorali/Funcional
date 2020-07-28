import Text.Show.Functions

import Data.List

data Cliente = Cliente {
    nombre :: String,
    categoria :: Char,
    saldo :: Float,
    ubicacion :: Coordenada
} deriving (Eq,Show)

type Coordenada = (Float, Float)

data Pedido = Pedido {
    cliente :: Cliente,
    productosSolicitados :: [Producto],
    fecha :: Fecha
} deriving (Eq,Show)

data Producto = Producto {
    descripcion :: String,
    precio :: Float,
    ubicacionDeposito :: Coordenada
} deriving (Eq,Show)
type Fecha = (Int, Int, Int)

--1-- Dado un conjunto de pedidos, obtener todos los pedidos viejos (del 2018 o anteriores)
anio (_,_,a) = a
esViejo pedido = (anio (fecha pedido)) < 2018
pedidosViejos listapedidos = filter esViejo listapedidos

--2-- Agregar un producto a un pedido
agregarProducto :: Pedido -> [Producto] -> Pedido
agregarProducto (Pedido cl pS fe) producto  = (Pedido cl (pS ++ producto) fe)

--3-- Obtener el cliente que está más lejos de una ubicación dada, de entre dos clientes dados. 
ubicacionespecifica :: Coordenada -> Coordenada -> Float
ubicacionespecifica (x,y) (x2, y2) = sqrt ((x2-x)^2 + (y2-y)^2)
--dadas dos ubicaciones, me devuelve un Flotante que representa cantidad de km de lejania

mayorDistancia cli1 cli2 ubi | ubicacionespecifica (ubicacion cli1) ubi > ubicacionespecifica (ubicacion cli2) ubi = cli1
                             | otherwise = cli2

--4-- Calcular el costo basico de un pedido
precioProducto :: Producto -> Float
precioProducto (Producto d precio ud) = precio

costoBasico :: Pedido -> Float
costoBasico pedido = sum (listadeCostos pedido)

listadeCostos :: Pedido -> [Float]
listadeCostos pedido = map precioProducto (productosSolicitados pedido)

--5-- Obtener el costo de envío de un pedido, asumiendo que se calcula como 3$ por cada km recorrido y considerando la distancia
--lineal entre el depósito y la ubicación del cliente.
kmrecorridos :: Cliente -> [Producto] -> Float
kmrecorridos cl produ = sum (map (trayectoria cl) produ)

trayectoria :: Cliente -> Producto -> Float
trayectoria cl produ = ubicacionespecifica (ubicacion cl) (ubicacionDeposito produ)

costoenvio :: Pedido -> Float
costoenvio pedido = kmrecorridos (cliente pedido) (productosSolicitados pedido) * 3

--6--Obtener el costo total de un pedido, que es el costo básico más el costo de envío, pero con un descuento de acuerdo a la categoría
-- del cliente. Los clientes comunes no tienen descuento, los clientes frecuentes tienen un 10% y los clientes que mas que clientes
-- son amigos, un 20%, pero sólo se aplica si el costo básico es mayor que el costo de envío.
costoTotal :: Pedido -> Float
costoTotal pedido | (aplicaDescuento pedido) = (costoBasico pedido + costoTotal pedido) * (catedescuento (cliente pedido))
                  | otherwise = costoBasico pedido + costoTotal pedido

catedescuento cliente | (categoria cliente) == 'f' = 0.9
                      | (categoria cliente) == 'a' = 0.8
                      | otherwise = 1
aplicaDescuento pedido = costoBasico pedido > costoenvio pedido 

--7--Saber como queda el cliente luego de haber pagado un pedido
estadocliente :: Cliente -> Pedido -> Cliente
estadocliente (Cliente n c saldo u) pedido = (Cliente n c (saldo - (costoTotal pedido)) u)

--8--Obtener el cliente con mayor deuda, de entre un conjunto de clientes.
menorsaldo :: Cliente -> Cliente -> Cliente
menorsaldo cl1 cl2 | saldo cl2 > saldo cl1 = cl1
                   | otherwise = cl2

mayordeudor :: [Cliente] -> Cliente
mayordeudor clientes = foldl1 menorsaldo clientes

--Defino algunos ejemplos para probar consola
--Clientes
omar = Cliente {
    nombre = "Omar",
    categoria = 'a',
    saldo = 12500,
    ubicacion = (100, 10)
}

marcelo = Cliente {
    nombre = "Marcelo",
    categoria = 'b',
    saldo = 15000,
    ubicacion = (16, 6)
}

mauro = Cliente {
    nombre = "Mauro",
    categoria = 'c',
    saldo = 3000,
    ubicacion = (15, 30)
}
--Fin Clientes

--Pedido--
pedido1 = Pedido {
    cliente = marcelo,
    productosSolicitados = [calculadora, botines],
    fecha = (26,6,2011)
}
pedido2 = Pedido {
    cliente = omar,
    productosSolicitados = [escoba, peluche],
    fecha = (1,3,2019)
}


peluche = Producto {
    descripcion = "Abrazable y cariñoso",
    precio = 750,
    ubicacionDeposito = (3,15)
}

escoba = Producto {
    descripcion = "La que mejor limpia",
    precio = 200,
    ubicacionDeposito = (2,190)
}

calculadora = Producto {
    descripcion = "Calcula a la perfeccion",
    precio = 1000,
    ubicacionDeposito = (10,1)
}

botines = Producto {
    descripcion = "Hacele goles al que quieras con estos botines",
    precio = 4500,
    ubicacionDeposito = (1,25)
}
--Fin pedido

--Lista con pedidos
listapedidos = [pedido1, pedido2]
--Fin lista con pedidos
