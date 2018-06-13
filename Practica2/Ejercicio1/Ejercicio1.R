# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
# Un paquete puede tener elementos visible o invisibles al usuario. Con el comando
# ls podemos mostrar todos los elementos, por defecto solo muestra los visibles.
# Haciendo uso de la función help, vemos que podemos motrar todos los elementos con all.names
 
help(ls)
# ls retorna un vector de string con el nombre de cada elemento del paquete.
# Hacemos uso de las propiedades del vector para ver los primeros 20

x <- ls("package:methods", all.names = TRUE)
x[1:20]

# Total de elementos:
length(x) # [1] 385

# Respuesta(b)
# Con la función environment() podemos averiguar en qué paquete se encuentran cada una de las siguientes funciones:
environment(read.table) # <environment: namespace:utils>
environment(data)       # <environment: namespace:utils>
environment(matrix)     # <environment: namespace:base>
environment(jpeg)       # <environment: namespace:grDevices>

# Respuesta(c)
# Podemos igualar el contenido del vector de cadenas de caracteres con la función que queramos.
# Al ejecutar el siguiente comando vemos que todos los valores son FALSE excepto uno, lo cual indica
# que "smoothScatter" se encuantra en el paquete "graphics".

ls("package:graphics") == "smoothScatter"

# También podemos averiguar en qué posición se encuentra con which()
which(ls("package:graphics") == "smoothScatter") # [1] 71