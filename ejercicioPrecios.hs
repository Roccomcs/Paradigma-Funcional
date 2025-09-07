-- * aplicarDescuento: Dado un precio y un descuento, obtener el precio final con el descuento aplicado.
aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precioUnitario descuento = precioUnitario - (precioUnitario * descuento / 100)

-- * aplicarCostoDeEnvio: Dado un precio y un costo de envío, obtener el precio final una vez sumado el costo de envío.
aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio precioUnitario costoDeEnvio = precioUnitario + costoDeEnvio

-- * precioTotal: Dado un precio unitario, una cantidad, un descuento y un costo de envío calcular el precio total. Para eso, hay que calcular el precio unitario con descuento y 
-- * multiplicarlo por la cantidad. ¡No te olvides de agregar el precio del envío!
precioTotal :: Float -> Float -> Float -> Float -> Float
precioTotal precioUnitario cantidad descuento costoDeEnvio =  aplicarCostoDeEnvio ((aplicarDescuento precioUnitario descuento) * cantidad) costoDeEnvio
-- ~ Utilizamos fromIntegral para convertir el Int a Float, ya que no se puede multiplicar un Float por un Int directamente.

-- * entregaSencilla: Una entrega es sencilla, si se hace en un día sencillo. Los días sencillos son lo que tienen una cantidad de letras par en el nombre.
entregaSencilla :: String -> Bool
entregaSencilla unDia = even (length unDia)
-- & entregaSencilla unDia = even (length . filter (`notElem` " /") $ unDia) Funcion que no cuenta los espacios
-- ~ even verifica si un valor es par o impar, devuelve True si es par y False si es impar.
-- ~ length cuenta la cantidad de caracteres de un string.

-- * esProductoDeLujo: Dado el nombre de un producto, saber si es de lujo. Un producto es de lujo cuando contiene una “x” o “z” en su nombre.
esProductoDeLujo :: String -> Bool
esProductoDeLujo unProducto = elem 'x' unProducto || elem 'z' unProducto || elem 'X' unProducto || elem 'Z' unProducto
-- & esProductoDeLujo unProducto = any (`elem` "xzXZ") unProducto
 
-- * esProductoDeElite: Un producto es de elite si es de lujo, codiciado y no es un producto corriente. 
esProductoCodiciado :: String -> Bool
esProductoCodiciado unProducto = length unProducto > 10

-- * esProductoCorriente: Dado el nombre de un producto, saber si es un producto corriente. Un producto es corriente si la primera letra de su nombre es una vocal.
esProductoCorriente :: String -> Bool
esProductoCorriente unProducto = elem (head unProducto) "aeiouAEIOU"
-- ~ head devuelve el primer elemento de una lista, en este caso la primera letra del nombre del producto.
-- ~ elem verifica si un elemento está en una lista, en este caso si la primera letra del nombre del producto es una vocal.

-- ^ Otra forma de hacerlo pero verificando el caracter vacio
{-  
    esProductoCorriente :: String -> Bool
    esProductoCorriente "" = False
    esProductoCorriente (x:_) = elem x "aeiouAEIOU"
-}

-- * esProductoDeElite: Un producto es de elite si es de lujo, codiciado y no es un producto corriente.
esProductoDeElite :: String -> Bool
esProductoDeElite unProducto = esProductoDeLujo unProducto && esProductoCodiciado unProducto && not (esProductoCorriente unProducto)

-- * productoXL: Dado un producto, conseguir su versión XL. Esta se consigue agregando ‘XL’ al final del nombre.
productoXL :: String -> String
productoXL unProducto = unProducto ++ " XL"
-- ~ ++ concatena dos strings, en este caso el nombre del producto con " XL".

-- * descodiciarProducto: Dado el nombre de un producto, generar uno que no sea codiciado. Para esto le vamos a sacar las últimas letras hasta que la cantidad de letras en el 
-- * nombre quede igual a 10 (ó menor a 10 en productos con nombres cortos)
descodiciarProducto :: String -> String
descodiciarProducto unProducto = take 10 unProducto
-- ~ take toma los primeros 10 caracteres de un string, si hay menos de 10 caracteres, toma todos los que hay.

-- * versionBarata: Dado el nombre de un producto conseguir su versión barata. La misma es el producto descodiciado y con su nombre dado vuelta.
versionBarata :: String -> String   
versionBarata unProducto = reverse (descodiciarProducto unProducto)
-- ~ reverse invierte el orden de los caracteres de un string, en este caso el nombre del producto descodiciado.










