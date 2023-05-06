import random
import sys

#tener en cuenta que contamos el usuario 0

cantidadDeUsuarios = 101
CantUsuario = ""
DatosUsuario = ""
Usuarios = []

CantRelacion = ""
DatoRelacion = ""
Relaciones = []
   
with open("Usuarios.txt", "w") as archivo:
    # Redirigir la salida estándar al archivo
    sys.stdout = archivo

    # Definir la lista que se va a imprimir
    nombres = ['"Sophia"', '"Jackson"', '"Olivia"', '"Liam"', '"Emma"', '"Noah"', '"Ava"', '"Aiden"', '"Isabella"', '"Caden"', '"Mia"', '"Grayson"', '"Riley"', '"Lucas"', '"Avery"', '"Mason"', '"Ella"', '"Logan"', '"Lily"', '"Ethan"', '"Hannah"', '"Oliver"', '"Chloe"', '"Jacob"', '"Evelyn"', '"Levi"', '"Aubrey"', '"William"', '"Addison"', '"James"', '"Natalie"', '"Alexander"', '"Sofia"', '"Michael"', '"Harper"', '"Benjamin"', '"Aaliyah"', '"Elijah"', '"Arianna"', '"Daniel"', '"Elizabeth"', '"Matthew"', '"Victoria"', '"Cameron"', '"Madelyn"', '"Luna"', '"Caleb"', '"Grace"', '"Dylan"', '"Aurora"', '"Luke"', '"Audrey"', '"Miles"', '"Bella"', '"Gabriel"', '"Brooklyn"', '"Anthony"', '"Zoe"', '"Jaxon"', '"Scarlett"', '"Ezra"', '"Claire"', '"Owen"', '"Skylar"', '"Isaiah"', '"Lila"', '"Jayden"', '"Leah"', '"Sebastian"', '"Gabriella"', '"Christian"', '"Naomi"', '"Jeremiah"', '"Alice"', '"Julian"', '"Adeline"', '"Hunter"', '"Aria"', '"Landon"', '"Eleanor"', '"Adrian"', '"Violet"', '"Thomas"', '"Caroline"', '"Eli"', '"Stella"', '"Nathan"', '"Savannah"', '"Isabelle"', '"Mateo"', '"Genesis"', '"Max"', '"Penelope"', '"Josiah"', '"Hazel"', '"Nolan"', '"Ellie"']

    # Imprimir cada elemento de la lista en una línea separada
    for i in range(0, cantidadDeUsuarios + 1, 1):
        CantUsuario = "usuario" + str(i)
        DatosUsuario = "(" + str(i) + ", " + str(nombres[random.randrange(0, 90)]) + ")"
        print(CantUsuario, " = ", DatosUsuario, sep='')
        Usuarios.append(CantUsuario)
        
        
    print(" ")
    
    for i in range(0, cantidadDeUsuarios, 1):
        CantRelacion = "relacion0_" + str(i+1)
        DatoRelacion = "(" + str(Usuarios[0]) + ", "+ str(Usuarios[i+1]) + ")"
        print(CantRelacion, " = ", DatoRelacion, sep='')
        Relaciones.append(CantRelacion)
        
    print(" ")

    print("relaciones = [", ", ".join(Relaciones), "]", sep="")
    print(" ")
    
    print("usuarios = [", ", ".join(Usuarios), "]", sep="")
    print(" ")
    
    print("publicaciones = []")
    print(" ")
    
    print("red = (usuarios, relaciones, publicaciones)")
    

    # Restaurar la salida estándar a la consola
    sys.stdout = sys.__stdout__