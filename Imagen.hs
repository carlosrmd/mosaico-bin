module Imagen
  ( hSplit, vSplit
  , colorPromedio
  )
  where

import Graphics.Mosaico.Imagen (Color(Color, rojo, verde, azul), Imagen(Imagen, altura, anchura, datos))



subImagen
  :: Integer -> Integer
  -> Integer -> Integer
  -> Imagen -> Imagen

subImagen xInicial yInicial anchura' altura' (Imagen {anchura = an, altura = al, datos = dat})
		= Imagen { anchura = anchura', altura  = altura', datos = fmap doit (take (fromIntegral altura') (drop (fromIntegral (yInicial)) dat)) }
				where doit lista = take (fromIntegral anchura') (drop (fromIntegral (xInicial)) lista)


hSplit :: Imagen -> (Imagen, Imagen)
hSplit (Imagen {anchura = an, altura = al, datos = dat}) = ( subImagen 0 0 an nuevaAltura (Imagen {anchura = an, altura = al, datos = dat}) ,
															 subImagen 0 nuevaAltura an (al-nuevaAltura) (Imagen {anchura = an, altura = al, datos = dat}) )
															where nuevaAltura = al `div` 2

vSplit :: Imagen -> (Imagen, Imagen)
vSplit (Imagen {anchura = an, altura = al, datos = dat})	 = ( subImagen 0 0 nuevaAnchura al (Imagen {anchura = an, altura = al, datos = dat}),
															 subImagen nuevaAnchura 0 (an-nuevaAnchura) al (Imagen {anchura = an, altura = al, datos = dat}) )
															where nuevaAnchura = an `div` 2

sumarTripletaColores :: [Integer] -> [Integer] -> [Integer]
sumarTripletaColores l1 [] = l1
sumarTripletaColores [] l2 = l2
sumarTripletaColores l1 l2 = [a+b | (a,b) <- zip l1 l2]

extraerColor :: Color -> [Integer]
extraerColor Color {rojo = roj, verde = verd, azul = az} = [fromIntegral roj,fromIntegral verd,fromIntegral az]

extraerColores :: [Color] -> [Integer]
extraerColores [] = []
extraerColores (a:as) = sumarTripletaColores (extraerColor a) (extraerColores as)

sumarColores :: [[Color]] -> [Integer]
sumarColores [] = []
sumarColores (a:as) = sumarTripletaColores (extraerColores a) (sumarColores as)

colorPromedio :: Imagen -> Color
colorPromedio (Imagen {anchura = an, altura = al, datos = dat}) = Color {rojo  = round ( (fromIntegral (head listaSumaColores)) / fromIntegral (an*al) ) ,
																		 verde = round ( (fromIntegral (head (drop 1 listaSumaColores) )) / fromIntegral (an*al) ) ,
																		 azul  = round ( (fromIntegral (head (drop 2 listaSumaColores) )) / fromIntegral (an*al) ) }
																	where listaSumaColores = sumarColores dat