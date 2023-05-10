module TesteoPropio where
import Test.HUnit
import Solucion

main = runTestTT testsMain

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

    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redB) ~?= ["Juan","Pedro", "Natalia"]

    ]

testsEjercicio2 = test [

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4]

    ]

testsEjercicio3 = test [

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2

    ]

testsEjercicio4 = test [

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4]


    ]

testsEjercicio5 = test [

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,
    " estaRobertoCarlos 2" ~: (estaRobertoCarlos redCarlos) ~?= True

    ]

testsEjercicio6 = test [

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2]


    ]

testsEjercicio7 = test [

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1]


    ]

testsEjercicio8 = test [

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True


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

usuariosCarlos = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesCarlos = [relacion1_2, relacion2_3, relacion11_1, relacion11_2, relacion11_3, relacion11_4, relacion11_5, relacion11_6, relacion11_7, relacion11_8, relacion11_9, relacion11_10, relacion11_12]
publicacionesCarlos = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redCarlos = (usuariosCarlos, relacionesCarlos, publicacionesCarlos)

