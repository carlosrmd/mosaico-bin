module Main (main) where

import Graphics.Mosaico.Diagrama (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo))
import Graphics.Mosaico.Imagen   (Imagen(Imagen, altura, anchura, datos), leerImagen)
import Graphics.Mosaico.Ventana  (Ventana, cerrar, crearVentana, leerTecla, mostrar)

import Diagramas (Orientación(Horizontal, Vertical), caminar, dividir, rectánguloImagen, sustituir)
import System.Environment

ciclo :: Ventana -> Diagrama -> [Paso] -> IO ()
ciclo v d pasos = do
					mostrar v (reverse pasos) d
					teclaleida <- leerTecla v;
					case teclaleida of
						Just tecla ->
							case tecla of
								"Up" 		-> do
									putStrLn "Up!";
									case (caminar pasos d) of
										Nothing ->
											putStrLn ""
										Just nuevoDiagram -> do
											case nuevoDiagram of
												Hoja r -> do
													case (dividir Horizontal r) of
														Just dia -> do
															ciclo v (sustituir dia pasos d) (pasos ++ [Primero])
														Nothing -> do
															ciclo v d pasos
												_ :-: _ -> do
													ciclo v d (pasos ++ [Primero])
												_		-> do
													ciclo v d pasos
								"Down" 		-> do
									putStrLn "Down!";
									case (caminar pasos d) of
										Nothing ->
											putStrLn ""
										Just nuevoDiagram -> do
											case nuevoDiagram of
												Hoja r -> do
													case (dividir Horizontal r) of
														Just dia -> do
															ciclo v (sustituir dia pasos d) (pasos ++ [Segundo])
														Nothing -> do
															ciclo v d pasos
												_ :-: _ -> do
													ciclo v d (pasos ++ [Segundo])
												_		-> do
													ciclo v d pasos
								"Left" 	-> do
									putStrLn "Left!";
									case (caminar pasos d) of
										Nothing ->
											putStrLn ""
										Just nuevoDiagram -> do
											case nuevoDiagram of
												Hoja r -> do
													case (dividir Vertical r) of
														Just dia -> do
															ciclo v (sustituir dia pasos d) (pasos ++ [Primero])
														Nothing -> do
															ciclo v d pasos
												_ :|: _ -> do
													ciclo v d (pasos ++ [Primero])
												_		-> do
													ciclo v d pasos
								"Right" 		-> do
									putStrLn "Right!";
									case (caminar pasos d) of
										Nothing ->
											putStrLn ""
										Just nuevoDiagram -> do
											case nuevoDiagram of
												Hoja r -> do
													case (dividir Vertical r) of
														Just dia -> do
															ciclo v (sustituir dia pasos d) (pasos ++ [Segundo])
														Nothing -> do
															ciclo v d pasos
												_ :|: _ -> do
													ciclo v d (pasos ++ [Segundo])
												_		-> do
													ciclo v d pasos
								"BackSpace" -> do
									putStrLn "BackSpace!";
									ciclo v d (take ((length pasos) - 1) pasos)
								"q"			-> do
									putStrLn "Bye";
								_ 			-> do
									putStrLn "Alguna otra.";
									ciclo v d pasos
									
						Nothing ->
							putStrLn ""


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
						ciclo v (Hoja(rectánguloImagen imagen)) []
					Left razon -> putStrLn razon
			_ -> putStrLn "Error en la entrada: Número de argumentos inválido."
		return ()
