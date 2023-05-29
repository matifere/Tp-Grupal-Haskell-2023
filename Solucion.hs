-- Completar con los datos del grupo
--
-- Nombre de Grupo: CS1.6NoSteam
-- Integrante 1: Guillermo Francisco Cozza, guiczza@gmail.com, 560/23
-- Integrante 2: Matías Ferechian, matifere@gmail.com, 693/23
-- Integrante 3: Thiago Ghianni, ghiannithiago@gmail.com, 1182/82
-- Integrante 4: Fabrizio Serraiocco, fabrizioserraiocco@gmail.com, 677/23
module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios (us, _, _) = sinRepetidos (nombreDeUsuario (head us) : nombresDeUsuarios ((tail us), [], []))

-- describir qué hace la función: devuelve la lista de amigos de un usuario de una red social.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = []
amigosDe (_, rels, _) u | (fst (head rels) == u) = sinRepetidos (snd (head rels) : amigosDe ([], (tail rels), []) u)
                        | (snd (head rels) == u) = sinRepetidos (fst (head rels) : amigosDe ([], (tail rels), []) u)
                        | otherwise = amigosDe ([], (tail rels), []) u

-- describir qué hace la función: devuelve la cantidad de amigos de un usuario de una red social.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = length (amigosDe red u)

-- describir qué hace la función: devuelve al usuario con más amigos de una red social dada.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red | length (usuarios red) == 1 = head (usuarios red)
                        | otherwise = quienTieneMasAmigos red (usuarios red)

-- describir qué hace la función: devuelve True si la red social dada tiene algún usuario con más (mayor estricto) de 10 amigos.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = (cantidadDeAmigos red (usuarioConMasAmigos red) > 10)


-- describir qué hace la función: devuelve la lista de publicaciones de un usuario de una red social.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, pubs) u  | length pubs == 0 = []
                                | u == (usuarioDePublicacion (head pubs)) = sinRepetidos (head pubs : (publicacionesDe ([], [], tail pubs) u))
                                | otherwise = (publicacionesDe ([], [], tail pubs) u)

-- describir qué hace la función: devuelve la lista de publicaciones de una red social que le gustan a un usuario.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (_, _, pubs) usr  | pertenece (likesDePublicacion (head pubs)) usr = sinRepetidos ((head pubs : publicacionesQueLeGustanA ([], [], (tail pubs)) usr))
                                            | otherwise = publicacionesQueLeGustanA ([], [], (tail pubs)) usr


-- describir qué hace la función: devuelve True si las listas de publicaciones que les gustan a dos usuarios son idénticas
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = (publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2)


-- describir qué hace la función: devuelve True si existe otro usuario en la red social que dio like a todas las publicaciones del usuario (n° publicaciones > 0).
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u   | publicacionesDe red u == [] = False
                            | otherwise = hayUnSeguidorFielEnLosLikes (likesDePublicacion (head (publicacionesDe red u))) (likesDePublicaciones (publicacionesDe red u)) u red


-- describir qué hace la función: Dados una red social y dos usuarios, devuelve True si existe una cadena de amistades que relaciona directa o indirectamente a los dos usuarios.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuario1 usuario2   | usuario1 == usuario2 = False
                                                | otherwise = hayCadenaDeAmistad red usuario2 [] [usuario1]




-- Funciones Auxiliares

--Esta función auxiliar elimina todas las repeticiones de elementos en una lista 
sinRepetidos :: (Eq t) => [t] -> [t]
sinRepetidos list   | length list == 0 = []
                    | (pertenece (tail list) (head list)) == True = sinRepetidos (tail list)
                    | otherwise = [head list] ++ sinRepetidos (tail list)

--Esta función auxiliar toma una lista genérica y un elemento del mismo tipo y devuelve True si el elemento está en la lista
pertenece :: (Eq t) => [t] -> t -> Bool
pertenece [] _ = False
pertenece list y    | head list == y = True
                    | otherwise = pertenece (tail list) y

--Esta función auxiliar toma una lista de listas genéricas y un elemento del mismo tipo y devuelve True si su el elemento está en todas las listas de la lista de listas
estaEnTodasLasListas :: (Eq t) => [[t]] -> t -> Bool
estaEnTodasLasListas listas y   | length listas == 0 = False
                                | (tail listas) == [] = pertenece (head listas) y
                                | pertenece (head listas) y = estaEnTodasLasListas (tail listas) y
                                | otherwise = False



--Esta funcion auxiliar se encarga de comparar que usuario tiene mas amigos dentro de una lista
quienTieneMasAmigos :: RedSocial -> [Usuario] -> Usuario
quienTieneMasAmigos red (x:y:xs)    | length (xs) == 0 && cantidadDeAmigos red x >= cantidadDeAmigos red y = x
                                    | length (xs) == 0 && cantidadDeAmigos red x < cantidadDeAmigos red y = y
                                    | cantidadDeAmigos red x >= cantidadDeAmigos red y = quienTieneMasAmigos red (x:xs)
                                    | cantidadDeAmigos red x < cantidadDeAmigos red y = quienTieneMasAmigos red (y:xs)

--Esta función auxiliar devuelve una lista compuesta por todas las listas de likes de una lista de publicaciones
likesDePublicaciones :: [Publicacion] -> [[Usuario]]
likesDePublicaciones pubs   | length pubs == 0 = []
                            | otherwise = [likesDePublicacion (head pubs)] ++ likesDePublicaciones (tail pubs) 

--Esta función auxiliar devuelve True si al menos uno de los de la lista de usuarios está en todas las listas de likes usuarios del segundo parametro,
--es distinto del creador de la publicacion y pertenece a los usuarios de la red
hayUnSeguidorFielEnLosLikes :: [Usuario] -> [[Usuario]] -> Usuario -> RedSocial -> Bool
hayUnSeguidorFielEnLosLikes usrs likesDePubs creador red    | length usrs == 0 = False
                                                            | (head usrs /= creador) && (estaEnTodasLasListas likesDePubs (head usrs)) && (pertenece (usuarios red) (head usrs)) = True
                                                            | otherwise = hayUnSeguidorFielEnLosLikes (tail usrs) likesDePubs creador red

--Esta funcion auxiliar recorre todos los posibles caminos para ver si se relacionan dos usuarios
hayCadenaDeAmistad :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
hayCadenaDeAmistad red usuario2 usrs_recorridos usrs_por_recorrer  | (usrs_por_recorrer == []) = False  
                                                                   | (head usrs_por_recorrer) == usuario2 = True
                                                                   | pertenece usrs_recorridos (head usrs_por_recorrer) = hayCadenaDeAmistad red usuario2 usrs_recorridos (tail usrs_por_recorrer) 
                                                                   | otherwise = hayCadenaDeAmistad red usuario2 ((head usrs_por_recorrer):usrs_recorridos) ((amigosDe red (head usrs_por_recorrer))++(tail usrs_por_recorrer))