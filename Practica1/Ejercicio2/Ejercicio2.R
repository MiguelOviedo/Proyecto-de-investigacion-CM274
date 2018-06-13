# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
# Se crea una matriz, definida con 4 filas y llenadas por fila
m <- matrix(c(4.3, 3.1, 8.2, 8.2, 3.2, 0.9, 1.6, 6.5), nrow = 4, byrow = TRUE)

# Respuesta(b)
# Al asignar la fila de la matriz con -4, le quitamos dicha fila manteniendo
# las columnas, entonces la dimensión es de 3x2.
m <- m[-4, ]

# Respuesta(c)
# Se modifica solo la columna 2 de la matriz con la misma columna, pero de manera
# ordenada con la función sort()
m[,2] <- sort(m[,2])

# Respuesta(d)
# R devuelve una matriz de 1x1 si el atributo drop (que elimina la columna y fila de la matriz) 
# está en FALSE
m <- m[-4, -1, drop = FALSE]

# Respuesta(e)
# Ahora se asigna a la matriz m2 las filas 3 y 4 de la matriz m.
# Se debe declarar ncol = 2 para que genere una matriz de 2x2
m2 <- matrix(c(m[3,], m[4,]), ncol = 2)

# Respuesta(f)
# Los elementos de la diagonal de m2 se asignan a la matriz m de la siguiente manera:
# Los elementos 1 y 4 de (1,4) se combinan con 2 de (2,1) para formar (1,2) y (4,2), luego con 1 para formar (1,1) y (4,1)
# Luego a (1,2) y (4,2) se le asignan (-1.6, -4.1), de igual forma para (1,1) y (4,1)
m[c(1,4),c(2,1)] <- diag(m2)*(-1/2)

# Respuesta(g)
# Realizando las siguientes operaciones se obtiene la matriz nula
A <- diag(c(2,3,5,-1), 4, 4) # crea la matiz de dimensión 4x4
inv_A <- solve(A) # solve() calcula la inversa de A
I_4 <- diag(4) # diag(4) crea una matriz identidad de 4x4
inv_A * A - I_4 # el resultado de esta operación es la matriz nula de 4x4
