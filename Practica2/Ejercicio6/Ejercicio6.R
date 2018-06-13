# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)

matlist <- list(matrix(c(T,F,T,T),2,2),
                matrix(c("a","c","b","z","p","q"),3,2),
                matrix(1:8,2,4))
matlist # muestra la lista

for(i in 1:length(matlist)){
  matlist[[i]] <- t(matlist[[i]]) # cambia cada matriz a su transpuesta
}

matlist # muestra la nueva lista

# con lapply podemos aplicar una función cualquiera a cada elemento de la lista
# que se pasa como primer parámetro.

lapply(matlist, t) # lapply opera la función transpuesta a cada elemento de la lista (matrices)
                   # y muestra el resultado

# Respuesta(b)

# Escribe un bucle implícito que obtenga los elementos diagonales de todas las matrices de segunda capa para producir la siguiente matriz:
qux <- array(96:1,dim=c(4,4,2,3))
qux

# cuando hay matrices de más de dos dimensiones se usa el segundo parámetro MARGIN = 3,
# para posicionarse a nivel de capas (1 para filas, 2 para columnas y 4 para bloques), y
# usamos qux[,,2,] para posicionarnos en la segunda capa, luego se aplica la función diagonal.
apply(qux[,,2,], 3, diag)

# Escribe un bucle implícito que devuelva las dimensiones de cada una de las tres matrices formadas accediendo a la cuarta columna de cada matriz en qux, 
# independientemente de la capa o bloque, enlazado por otro bucle implícito que encuentre la suma de filas de esa estructura, 
# resultando en el siguiente vector:

# [1] 12 6

# qux[,4,,] devuelve por cada capa por cada una matriz de 4x2, 
# cada columna es la cuarta columna de los bloques 1 y 2 por capa
dimensiones <- apply(qux[,4,,],3, dim)
dimensiones

#     [,1] [,2] [,3]
#[1,]    4    4    4
#[2,]    2    2    2

# aplicamos nuevamente apply sobre la matriz resultante sumando las filas (MARGIN = 1)
sumas <- apply(dimensiones, 1, sum)
sumas

# [1] 12 6