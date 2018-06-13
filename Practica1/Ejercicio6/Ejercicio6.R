# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
# usamos la función write.table para exportar los datos en el archivo "q5.txt"
# sin nombres de fila y sin comillas en el atributo mag.
write.table(subset(quakes["mag"], mag > 5), file="q5.txt", sep="\t", quote = FALSE, row.names = FALSE)
# Leemos los datos  del archivo y lo almacenamos en la variable q5.dframe
q5.dframe <- read.table("q5.txt", TRUE)
q5.dframe # muestra los datos leidos

# Respuesta(b)
library(car) # cargamos la librería car para poder usar el dataframe Duncan

png(filename="Ejercicio6.png",width=500,height=500) # crea un archivo .png con un nombre, y dimensiones 500x500 píxeles
# plot carga los datos education e income con prestige <= 80, xlim e ylim indican el rango de los ejes X e Y.
# xlab, ylab y main indican las etiquetas de los ejes X, Y y principal respectivamente. pch indica el tipo de objeto que se dibujará

plot(Duncan$education[Duncan$prestige <= 80],Duncan$income[Duncan$prestige <= 80],
     xlim=c(0,100),ylim=c(0,100), ylab="income",xlab="education",main="education vs income", pch=1)
# points agrega puntos con prestige > 80, con el tipo de objeto pch = 1 (puntos negros)

points(Duncan$education[Duncan$prestige > 80],Duncan$income[Duncan$prestige > 80],pch=19)

legend("bottomright", legend = c("prestige <= 80", "prestige > 80"), pch = c(1, 19))

dev.off() # indica que termina de graficar (regresar al dispositivo)

# Respuesta(c)
# dput puede almacenar objetos que contengan datos heterogéneos como las listas, además de atributos.
# dget lee los datos tal y como se guardaron con dput.
exer <- list(quakes, q5.dframe, Duncan) # almacena la lista en la variable exer
dput(exer, file="Ejercicio_df.txt") # almacena la lista en dput

lista.de.dataframes <- dget(file="Ejercicio_df.txt") # lee los datos del archivo
lista.de.dataframes # muestra la lista exer
