module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)

import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)

ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo = undefined


main :: IO ()
main =
	do
		foo <- leerImagen "black.png"
		case foo of
			Right imagen -> do
				v <- crearVentana (anchura imagen) (altura imagen);
				mostrar v [] (Hoja(rectánguloImagen imagen))
			Left razon -> putStrLn razon
		return ()
