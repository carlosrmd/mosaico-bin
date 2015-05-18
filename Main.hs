module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)

import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)
import System.Environment

ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo v d pasos = undefined


main :: IO ()
main =
	do
		args <- getArgs
		case args of
			[name] -> do
				foo <- leerImagen name
				case foo of
					Right imagen -> do
						v <- crearVentana (anchura imagen) (altura imagen);
						mostrar v [] (Hoja(rectánguloImagen imagen))
						ciclo v (Hoja(rectánguloImagen imagen)) []
					Left razon -> putStrLn razon
			_ -> putStrLn "Error en la entrada: Número de argumentos inválido."
		return ()
