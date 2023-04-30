-- Completar con los datos del grupo
--
-- Nombre de Grupo: CS1.6NoSteam
-- Integrante 1: Guillermo Francisco Cozza, guiczza@gmail.com, 560/23
-- Integrante 2: Matías Ferechian, matifere@gmail.com, 693/23
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

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
amigosDe red u = amigosDeAuxiliar (relaciones red) u

-- describir qué hace la función: devuelve la cantidad de amigos de un usuario dado de una red social dada.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = length (amigosDe red u)

-- describir qué hace la función: devuelve al usuario con más amigos de una red social dada.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined --A quién concierna a futuro, en esta implementación va a ser útil usar cantidadDeAmigos, que era otro que ya implementé.

-- describir qué hace la función: devuelve True si la red social dada tiene algún usuario con más (mayor estricto) de 1000000 amigos. Sino devuelve False.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: devuelve la lista de publicaciones de un usuario dado de una red social dada
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: devuelve la lista de publicaciones de una red social dada que le gustan a un usuario dado
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: devuelve True si las listas de publicaciones que les gustan a dos usuarios son idénticas (esto está bien interpretado de la especificación?)
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: devuelve True si existe otro usuario en la red social dada que dio like a todas las publicaciones del usuario dado (n° publicaciones > 0). Sino devuelve False.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: Dados una red social y dos usuarios, devuelve True si existe una cadena de amistades que relaciona directa o indirectamente a los dos usuarios. Sino devuelve false (esto está bien interpretado de la especificación?)
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined




-- Funciones Auxiliares

--Esta función auxiliar itera a través de una lista de usuarios y devuelve una lista con sólo los nombres
nombresDeUsuariosAuxiliar :: [Usuario] -> [String]
nombresDeUsuariosAuxiliar [] = [] 
nombresDeUsuariosAuxiliar us = nombreDeUsuario (head us) : nombresDeUsuariosAuxiliar (tail us)

--Esta función auxiliar itera a través de una lista de relaciones y devuelve una lista con los usuarios que estén en una relación con el usuario dado
amigosDeAuxiliar :: [Relacion] -> Usuario -> [Usuario]
amigosDeAuxiliar [] _ = []
amigosDeAuxiliar rels u | (fst (head rels) == u) = snd (head rels) : amigosDeAuxiliar (tail rels) u
                        | (snd (head rels) == u) = fst (head rels) : amigosDeAuxiliar (tail rels) u
                        | otherwise = amigosDeAuxiliar (tail rels) u