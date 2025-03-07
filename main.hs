{-|

Welcome to GDB Online.
GDB online is an online compiler and debugger tool for C, C++, Python, Java, PHP, Ruby, Perl,
C#, OCaml, VB, Swift, Pascal, Fortran, Haskell, Objective-C, Assembly, HTML, CSS, JS, SQLite, Prolog.
Code, Compile, Run and Debug online from anywhere in world.

-}
import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada id tiempo estudiantes =
    Estudiante id tiempo Nothing : estudiantes

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo estudiantes =
    map (\e -> if id == idEstudiante e then e { salida = Just tiempo } else e) estudiantes

-- Función para buscar un estudiante por su ID en la universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id estudiantes =
    find (\e -> id == idEstudiante e && isNothing (salida e)) estudiantes
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarEstudiantes :: [Estudiante] -> IO ()
guardarEstudiantes estudiantes = do
    withFile "estudiantes.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map show estudiantes))
    putStrLn "Registro guardado en el archivo estudiantes.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarEstudiantes :: IO [Estudiante]
cargarEstudiantes = do
    contenido <- withFile "estudiantes.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante id entrada salida) =
    "Estudiante {ID = " ++ id ++ ", Entrada = " ++ show entrada ++ ", Salida = " ++ maybe "Nada" show salida ++ "}"

-- Función para listar los estudiantes registrados
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes registrados en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    estudiantes <- cargarEstudiantes
    putStrLn "¡Bienvenido al Sistema de Registro de Estudiantes!"
    cicloPrincipal estudiantes

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estudiantes = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarEntrada idEst tiempoActual estudiantes
            putStrLn $ "Estudiante con ID " ++ idEst ++ " ha ingresado a la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "2" -> do
            putStrLn "Ingrese el ID del estudiante que sale:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarSalida idEst tiempoActual estudiantes
            putStrLn $ "Estudiante con ID " ++ idEst ++ " ha salido de la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEst <- getLine
            case buscarEstudiante idEst estudiantes of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ idEst ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal estudiantes

        "4" -> do
            listarEstudiantes estudiantes
            cicloPrincipal estudiantes

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal estudiantes
