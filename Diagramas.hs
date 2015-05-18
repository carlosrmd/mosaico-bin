module
  Diagramas
  ( rectánguloImagen
  , Orientación(Horizontal, Vertical)
  , dividir
  , caminar
  , sustituir
  )
  where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo), Rectángulo(Rectángulo, color, imagen))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos))

import Imagen (colorPromedio, hSplit, vSplit)



rectánguloImagen :: Imagen -> Rectángulo
rectánguloImagen imag = Rectángulo { color = colorPromedio imag , imagen = imag }

data Orientación  
  = Horizontal
  | Vertical
  deriving Show

dividir :: Orientación -> Rectángulo -> Maybe Diagrama
dividir Horizontal (Rectángulo { color = colorActual, imagen = imag }) = if ((altura imag) < 2) then
                                                                          Nothing
                                                                         else
                                                                          Just (( Hoja (rectánguloImagen (fst nuevaImagen)) ) :-: ( Hoja (rectánguloImagen (snd nuevaImagen)) ))
                                                                         where nuevaImagen = hSplit imag

dividir Vertical   (Rectángulo { color = colorActual, imagen = imag }) = if ((anchura imag) < 2) then
                                                                          Nothing
                                                                         else
                                                                          Just (( Hoja (rectánguloImagen (fst nuevaImagen)) ) :|: ( Hoja (rectánguloImagen (snd nuevaImagen)) ))
                                                                         where nuevaImagen = vSplit imag

caminar :: [Paso] -> Diagrama -> Maybe Diagrama
caminar [] diagram = Just diagram

caminar (paso:pasos) (diaa :-: diab) = case paso of
                                        Primero -> caminar pasos diaa
                                        Segundo -> caminar pasos diab

caminar (paso:pasos) (diaa :|: diab) = case paso of
                                          Primero -> caminar pasos diaa
                                          Segundo -> caminar pasos diab

caminar pasos (Hoja rect) = if null pasos then
                            Just (Hoja rect)
                           else
                            Nothing

sustituir :: Diagrama -> [Paso] -> Diagrama -> Diagrama
sustituir = undefined
