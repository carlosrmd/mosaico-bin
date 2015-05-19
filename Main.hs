module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)

import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)
import System.Environment

ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo v d pasos = do
					teclaleida <- leerTecla v;
					case teclaleida of
						Just tecla ->
							case tecla of
								"Up" 		-> do
									putStrLn "Up!";
									ciclo v d pasos
								"Down" 		-> do
									putStrLn "Down!";
									ciclo v d pasos
								"Right" 	-> do
									putStrLn "Right!";
									ciclo v d pasos
								"Left" 		-> do
									putStrLn "Left!";
									ciclo v d pasos
								"BackSpace" -> do
									putStrLn "BackSpace!";
									ciclo v d pasos
								"q"			-> do
									putStrLn "Bye";
								_ 			-> do
									putStrLn "Alguna otra.";
									ciclo v d pasos
									
						Nothing ->
							putStrLn "Culo"


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
