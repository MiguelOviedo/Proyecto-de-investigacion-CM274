# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
f1 <- c(13563,-14156,-14319,16981,12921,11979,9568,8833,-12968, 8133)

# Muestra la salida todos los elementos de f1 que, cuando se eleva a una potencia de 75, no son infinitos.
f1 <- f1^75
f1[-which(is.infinite(f1))]

# Devuelve los elementos de f1, excluyendo aquellos que resultan en infinito negativo cuando se eleva a una potencia de 75.
f1[which(f1 != -Inf)]

# Respuesta(b)
# Identifica los índices específicos de las entradas de varMatriz que son NaN cuando se eleva varMatriz a una potencia de 65 y se divide por infinito.
varMatriz <- matrix(c(77875.40, 27551.45, 23764.30, -36478.88, -35466.25,
                    -73333.85, 36599.69, -70585.69, -39803.81, 55976.34,
                    76694.82, 47032.00), ncol = 4,  byrow = TRUE)

varMatriz <- (varMatriz ^ 65) / Inf
# indx_matrix muestra la fila y columna en la que se encuentran los elementos NaN
indx_matrix <- which(is.nan(varMatriz), arr.ind=TRUE) # arr.ind muestra los índices

# Devuelve los valores en varMatriz que NO son NaN cuando se eleva varMatriz a una potencia de 67 y se añade infinito al resultado.
# Confirma que esto es idéntico a identificar aquellos valores en varMatriz que, cuando aumentan a una potencia de 67, no son iguales al infinito negativo.
varMatriz2 <- (varMatriz ^ 67) + Inf
varMatriz2[which(!is.nan(varMatriz2))] #[1] Inf Inf Inf Inf Inf Inf Inf Inf Inf Inf

varMatriz3 <- varMatriz ^ 67
# Verificamos que las dos líneas siguientes corresponden con lo pedido en el problema
which(varMatriz3 != -Inf, arr.ind = TRUE)
which(!is.nan(varMatriz2), arr.ind = TRUE)

# Identifique los valores en varMatriz que sean infinito negativo o finito cuando eleva varMatriz a una potencia de 67.
varMatriz <- (varMatriz ^ 67)
elem <- (!(is.nan(varMatriz) | is.infinite(varMatriz)) | varMatriz == -Inf)
varMatriz <- varMatriz[elem]
varMatriz

# Estos elementos están en las posiciones siguientes:
which(elem, arr.ind=TRUE)

# Respuesta(c)
f2 <- c(4.3,2.2,NULL,2.4,NaN,3.3,3.1,NULL,3.4,NA)
length(f2) # VERDADERO
f3 <- c(NULL,NULL, NULL, NA)
which(x = is.na(x=f2)) # VERDADERO

is.null(x=f2) # FALSO
# El resultado de este último resultado es curioso. Si el vector tiene solo
# NULL ,sin importar la cantidad, is.null es verdadero, pero basta que le agrege
# cualquier otro valor de cualquier tipo inclusive NaN o NA y ahora será falso.
# Eso se debe a que en un vector, NULL, se considera como algo que no existe y por eso
# lo obvia, considerando solo los demás tipos que no son NULL y por eso is.null sale siempre falso
