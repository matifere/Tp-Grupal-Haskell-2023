module TesteoPropio where
import Test.HUnit
import Solucion

main = runTestTT testsMain
ej1 = runTestTT testsEjercicio1
ej2 = runTestTT testsEjercicio2
ej3 = runTestTT testsEjercicio3
ej4 = runTestTT testsEjercicio4
ej5 = runTestTT testsEjercicio5
ej6 = runTestTT testsEjercicio6
ej7 = runTestTT testsEjercicio7
ej8 = runTestTT testsEjercicio8
ej9 = runTestTT testsEjercicio9
ej10 = runTestTT testsEjercicio10


testsMain = test [

    "testsEjercicio1" ~: testsEjercicio1,
    "testsEjercicio2" ~: testsEjercicio2,
    "testsEjercicio3" ~: testsEjercicio3,
    "testsEjercicio4" ~: testsEjercicio4,
    "testsEjercicio5" ~: testsEjercicio5,
    "testsEjercicio6" ~: testsEjercicio6,
    "testsEjercicio7" ~: testsEjercicio7,
    "testsEjercicio8" ~: testsEjercicio8,
    "testsEjercicio9" ~: testsEjercicio9,
    "testsEjercicio10" ~: testsEjercicio10

    ]

testsEjercicio1 = test [

    " nombresDeUsuarios sin repeticiones" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios nombres repetidos" ~: (nombresDeUsuarios redB) ~?= ["Juan","Pedro", "Natalia"],
    " nombresDeUsuarios red vacia" ~: (nombresDeUsuarios redVacia) ~?= []

    ]

testsEjercicio2 = test [

    " amigosDe con amigos" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    " amigosDe sin amigos" ~: (amigosDe redB usuario5) ~?= [] 

    ]

testsEjercicio3 = test [

    " cantidadDeAmigos con amigos" ~: (cantidadDeAmigos redA usuario1) ~?= 2,
    " cantidadDeAmigos sin amigos" ~: (cantidadDeAmigos redB usuario5) ~?= 0

    ]

testsEjercicio4 = test [

    " usuarioConMasAmigos red 'normal'" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    " usuarioConMasAmigos un solo usuario" ~: (usuarioConMasAmigos ([usuario1], [], [])) ~?= usuario1


    ]

testsEjercicio5 = test [

    " estaRobertoCarlos sin Roberto Carlos" ~: (estaRobertoCarlos redA) ~?= False,
    " estaRobertoCarlos con Roberto Carlos" ~: (estaRobertoCarlos redCarlos) ~?= True

    ]

testsEjercicio6 = test [

    " publicacionesDe con publicaciones" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],
    " publicacionesDe con publicaciones y repetidas" ~: (publicacionesDe (usuariosA, [], publicacionesA ++ [publicacion2_1, publicacion2_1, publicacion2_2]) usuario2) ~?= [publicacion2_1, publicacion2_2],
    " publicacionesDe sin publicaciones" ~: (publicacionesDe redB usuario5) ~?= []


    ]

testsEjercicio7 = test [

    " publicacionesQueLeGustanA con likes" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],
    " publicacionesQueLeGustanA sin likes" ~: (publicacionesQueLeGustanA (usuariosB, [], [publicacion3_1, publicacion3_3]) usuario1) ~?= [],
    " publicacionesQueLeGustanA sin publicaciones" ~: (publicacionesQueLeGustanA (usuariosB, [], []) usuario1) ~?= []

    ]

testsEjercicio8 = test [

    " lesGustanLasMismasPublicaciones sin likes" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,
    " lesGustanLasMismasPublicaciones con likes, distintas" ~: (lesGustanLasMismasPublicaciones redB usuario2 usuario5) ~?= False,
    " lesGustanLasMismasPublicaciones con likes, iguales" ~: (lesGustanLasMismasPublicaciones (usuariosB, [], [publicacion1_3, publicacion3_3]) usuario2 usuario5) ~?= True


    ]

testsEjercicio9 = test [

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True

    ]

testsEjercicio10 = test [

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True

    ]


expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Juan")
usuario7 = (7, "Natalia")
usuario8 = (8, "Pedro")
usuario9 = (9, "Mariela")
usuario10 = (10, "Natalia")
usuario11 = (11, "Roberto")
usuario12 = (12, "Juan")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

relacion11_1 = (usuario11, usuario1) --Amigos de Roberto
relacion11_2 = (usuario11, usuario2)
relacion11_3 = (usuario11, usuario3)
relacion11_4 = (usuario11, usuario4)
relacion11_5 = (usuario11, usuario5)
relacion11_6 = (usuario11, usuario6)
relacion11_7 = (usuario11, usuario7)
relacion11_8 = (usuario11, usuario8)
relacion11_9 = (usuario11, usuario9)
relacion11_10 = (usuario11, usuario10)
relacion11_12 = (usuario11, usuario12)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redVacia = ([], [], [])

usuariosCarlos = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesCarlos = [relacion1_2, relacion2_3, relacion11_1, relacion11_2, relacion11_3, relacion11_4, relacion11_5, relacion11_6, relacion11_7, relacion11_8, relacion11_9, relacion11_10, relacion11_12]
publicacionesCarlos = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redCarlos = (usuariosCarlos, relacionesCarlos, publicacionesCarlos)