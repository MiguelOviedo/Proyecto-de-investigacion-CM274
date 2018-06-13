# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
# La función seq() genera una secuencia de números con una progresión de 0.3
seq(5, -11, by = -0.3)
# el último número no es -11, ya que éste no es un término de la progresión, sin embargo
# se puede hacer el siguiente artificio:
c(-15:33)*(-1/3)

# Respuesta(b)
# Para tener una secuencia invertida se dispone el orden de los límites al revés
seq(-11, 5, by = 0.3)

c(33:-15)*(-1/3)

# Respuesta(c)
# Para obtener lo solicitado se almacena el vector en una variable v,
# se usa la función rep() cuyos parámetros indican el vector a replicar, las veces
# que se va a repetir el vector y la repetición de cada elemento respectivamente.
# El resultado se guarda en un variable res y luego se emplea la función order(),
# esta función devuelve los índices de los elementos ordenados de menor a mayor.
# Por último la función rev() invierte el orden de los elementos de mayor a menor.
v <- c(-1,3,-5,7,-9)
res <- rep(v, times = 2, each = 10)
res[order(res)]
rev(res[order(res)])

# Respuesta(d)
# Se crea un vector por cada condición y luego se concatena todo con la función c()
seq_ent <- seq(6, 12)
seq_rep <- rep(5.3, times = 3)
lim <- length(rev(res[order(res)]))
# -0.25 se obtiene calculando una progresión aritmética
seq_9 <- seq(102, lim, by = -0.25)
vector <- c(seq_ent, seq_rep, -3, seq_9)
vector
length(vector) # [1] 20