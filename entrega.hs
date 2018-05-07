module MicroEntrega1 where

import Text.Show.Functions
-- microprocesador [posiciones] acumuladorA acumuladorB contador etiqueta
main :: IO ()
main = return ()

--3.1 Punto 1: Modelar micro
data Microprocesador = Microprocesador [Int] Int Int Int String deriving (Show)
xt8088 = Microprocesador [] 0 0 0 ""
programCounter (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) = contador
acumuladorA (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) = acumuladorA
acumuladorB (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) = acumuladorB
mensajeError (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) = etiqueta
memoria (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) = posiciones

--3.2 Punto 2
aumentarPc micro = micro {programCounter = ((+1).programCounter) micro }
nop = aumentarPc 
--3.3 Punto 3
lodV (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) valor  = Microprocesador posiciones valor acumuladorB (contador+1) etiqueta
swap (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta )  = Microprocesador posiciones acumuladorB acumuladorA (contador+1) etiqueta
add (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) = Microprocesador posiciones (acumuladorA+acumuladorB) 0 (contador+1) etiqueta
sumar valor1 valor2 (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) = add (lodV ((swap ((lodV (Microprocesador  posiciones acumuladorA acumuladorB contador etiqueta) valor1 )))) valor2)

--3.4 Punto 4

divide (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta )  = Microprocesador posiciones (div acumuladorA acumuladorB) 0 (contador+1) etiqueta
str (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) pos valor  = Microprocesador ((take (pos -1) posiciones) ++ [valor] ++ drop pos posiciones) acumuladorA acumuladorB (contador+1) etiqueta
lod (Microprocesador posiciones acumuladorA acumuladorB contador etiqueta ) valor = Microprocesador (posiciones ++ [valor]) acumuladorA acumuladorB (contador+1) etiqueta


avanzar3 micro = (nop.nop.nop) micro
fp20 = Microprocesador [] 7 24 0 ""
at8086 = Microprocesador [1..20] 0 0 0 ""

