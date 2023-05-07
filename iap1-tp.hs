-- Completar con los datos del grupo
--
-- Nombre de Grupo: CS1.6NoSteam
-- Integrante 1: Guillermo Francisco Cozza, guiczza@gmail.com, 560/23
-- Integrante 2: Matías Ferechian, matifere@gmail.com, 693/23
-- Integrante 3: Thiago Ghianni, ghiannithiago@gmail.com, 1182/82
-- Integrante 4: Fabrizio Serraiocco, fabrizioserraiocco@gmail.com, 677/23

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
nombresDeUsuarios red = nombresDeUsuariosAuxiliar (usuarios red)

-- describir qué hace la función: devuelve la lista de amigos de un usuario dado de una red social dada.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = sinRepetidos (amigosDeAuxiliar (relaciones red) u)

-- describir qué hace la función: devuelve la cantidad de amigos de un usuario dado de una red social dada.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = length (amigosDe red u)

-- describir qué hace la función: devuelve al usuario con más amigos de una red social dada.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (usuarios, relaciones, publicaciones) = masAmigos (usuarios, relaciones, publicaciones) usuarios

-- describir qué hace la función: devuelve True si la red social dada tiene algún usuario con más (mayor estricto) de 1000000 amigos. Sino devuelve False.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (usuarios, relaciones, publicaciones) = chequearCantidadDeAmigos (usuarios, relaciones, publicaciones) usuarios

-- describir qué hace la función: devuelve la lista de publicaciones de un usuario dado de una red social dada
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: devuelve la lista de publicaciones de una red social dada que le gustan a un usuario dado
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (usuarios, relaciones, publicaciones) usr = leGustaLaPublicacion publicaciones usr

-- describir qué hace la función: devuelve True si las listas de publicaciones que les gustan a dos usuarios son idénticas (esto está bien interpretado de la especificación?)
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

--DESCOMENTAR Y CHEQUEAR QUE ESTO ANDE CUANDO ESTE HECHO publicacionesDe. DESPUES MOVER LA FUNCION AUXILIAR PARA ABAJO
{-
-- describir qué hace la función: devuelve True si existe otro usuario en la red social dada que dio like a todas las publicaciones del usuario dado (n° publicaciones > 0). Sino devuelve False.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u   | publicacionesDe red u == [] = False
                            | otherwise = tieneUnSeguidorFielAuxiliar (likesDePublicacion (head (publicacionesDe red u))) (likesDePublicaciones (publicacionesDe red u)) u red

--Esta función auxiliar devuelve True si al menos uno de los de la lista de usuarios está en todas las listas de usuarios del segundo parametro
--(es decir, le dio meGusta a todas las publicaciones de la lista de publiciones), es distinto del creador de la publicacion (tercer param) y pertenece a los usuarios de la red (cuarto param)
tieneUnSeguidorFielAuxiliar :: [Usuario] -> [[Usuario]] -> Usuario -> RedSocial -> Bool
tieneUnSeguidorFielAuxiliar usrs likesDePubs creador red    | length usrs == 0 = False
                                                            | (head usrs /= creador) && (estaEnTodasLasListas likesDePubs (head usrs)) && (pertenece (usuarios red) (head usrs)) = True
                                                            | otherwise = tieneUnSeguidorFielAuxiliar (tail usrs) likesDePubs creador red
-}


-- describir qué hace la función: Dados una red social y dos usuarios, devuelve True si existe una cadena de amistades que relaciona directa o indirectamente a los dos usuarios. Sino devuelve False (esto está bien interpretado de la especificación?)
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuario1 usuario2 = existeSecuenciaDeAmigosAuxiliar red  usuario2 [] [usuario1] 




-- Funciones Auxiliares

--Esta función auxiliar elimina todas las repeticiones de una lista
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


--Esta función auxiliar itera sobre de una lista de usuarios y devuelve una lista con sólo los nombres
nombresDeUsuariosAuxiliar :: [Usuario] -> [String]
nombresDeUsuariosAuxiliar [] = [] 
nombresDeUsuariosAuxiliar us = nombreDeUsuario (head us) : nombresDeUsuariosAuxiliar (tail us)

--Esta función auxiliar itera sobre de una lista de relaciones y devuelve una lista con los usuarios que estén en una relación con el usuario dado
amigosDeAuxiliar :: [Relacion] -> Usuario -> [Usuario]
amigosDeAuxiliar [] _ = []
amigosDeAuxiliar rels u | (fst (head rels) == u) = snd (head rels) : amigosDeAuxiliar (tail rels) u
                        | (snd (head rels) == u) = fst (head rels) : amigosDeAuxiliar (tail rels) u
                        | otherwise = amigosDeAuxiliar (tail rels) u

--Esta funcion auxiliar se encarga de comparar que usuario tiene mas amigos dentro de una lista
masAmigos :: RedSocial -> [Usuario] -> Usuario
masAmigos red (x:y:xs) | length (xs) == 0 && cantidadDeAmigos red x >= cantidadDeAmigos red y = x
                       | length (xs) == 0 && cantidadDeAmigos red x < cantidadDeAmigos red y = y
                       | cantidadDeAmigos red x >= cantidadDeAmigos red y = masAmigos red (x:xs)
                       | cantidadDeAmigos red x < cantidadDeAmigos red y = masAmigos red (y:xs)


--Esta funcion auxiliar itera sobre una lista de publicaciones, y devuelve una lista con solo aquellas que contengan a un cierto usuario
leGustaLaPublicacion :: [Publicacion] -> Usuario -> [Publicacion]
leGustaLaPublicacion [] _ = []
leGustaLaPublicacion pubs usr   | pertenece (likesDePublicacion (head pubs)) usr = ((leGustaLaPublicacion (tail pubs)) usr) ++ [head pubs]
                                | otherwise = leGustaLaPublicacion (tail pubs) usr
                     
--Esta funcion auxiliar itera sobre todos los usuarios de una red chequeando si tienen mas de 1000000 de amigos
chequearCantidadDeAmigos :: RedSocial -> [Usuario] -> Bool
chequearCantidadDeAmigos _ [] = False
chequearCantidadDeAmigos k (x:xs) | cantidadDeAmigos k x > 1000000 = True
                                  | otherwise = chequearCantidadDeAmigos k xs

--Esta función auxiliar devuelve una lista compuesta en todas las listas de likes de una lista de publicaciones
likesDePublicaciones :: [Publicacion] -> [[Usuario]]
likesDePublicaciones pubs   | length pubs == 0 = []
                            | otherwise = [likesDePublicacion (head pubs)] ++ likesDePublicaciones (tail pubs)

--Esta funcion auxiliar recorre todos los posibles caminos para ver si se relacionan dos usuarios
existeSecuenciaDeAmigosAuxiliar :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
existeSecuenciaDeAmigosAuxiliar red usuario2 usrs_recorridos usrs_por_recorrer  | (usrs_por_recorrer == []) = False  
                                                                                | (head usrs_por_recorrer) == usuario2 = True
                                                                                | pertenece usrs_recorridos (head usrs_por_recorrer) = existeSecuenciaDeAmigosAuxiliar red usuario2 usrs_recorridos (tail usrs_por_recorrer) 
                                                                                | otherwise = existeSecuenciaDeAmigosAuxiliar red usuario2 ((head usrs_por_recorrer):usrs_recorridos) ((amigosDe red (head usrs_por_recorrer))++(tail usrs_por_recorrer)) 
