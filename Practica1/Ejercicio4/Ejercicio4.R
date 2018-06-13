# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# # Respuesta(a)
# Las clases son explícitas si se definen atributos que facilitan el entendimiento de los objetos en si y son opcionales
# Las clases son implícitas si en éstas no se identifican atributos
f1 <- array(data=1:36, dim=c(3,3,4))
class(f1)   # [1] "array"
# Clase definida explícitamente

# Las coerciones de objetos tambíen se dan explícita o implícitamente
# Un objeto está coercionado de manera implícita cuando hay una correspondencia 
# de tipo de dato y la coerción es natural.
# Un objeto está coercionado de manera explícita cuando es necesario usar las funciones as.dot
# ya que los tipos de datos son de naturaleza distinta

bar <- as.vector(f1)
class(bar)  # [1] "integer"
class(f1)   # [1] "array"
# Definido explícitamente

baz <- as.character(bar)
class(baz)  # [1] "character"
class(bar)  # [1] "integer"
# Definido explícitamente

qux <- as.factor(baz)
class(qux)  # [1] "factor"
class(baz)  # [1] "character"
# Definido explícitamente

quux <- bar + c(-0.1,0.1)
class(quux) # [1] "numeric"
class(bar)  # [1] "integer"
# Definido implícitamente

# # Respuesta(b)
res_f1 <- is.numeric(f1) + is.integer(f1)
res_bar <- is.numeric(bar) + is.integer(bar)
res_baz <- is.numeric(baz) + is.integer(baz)
res_qux <- is.numeric(qux) + is.integer(qux)
res_quux <- is.numeric(quux) + is.integer(quux)

resultados <- c(res_f1, res_bar, res_baz, res_qux, res_quux)
fac <- factor(resultados, levels = c(0, 1, 2), ordered = TRUE)
fac
as.vector(resultados)
class(resultados)
class(as.vector(resultados))

# El resultado de coercer el factor fac a vector da el mismo resultado, ya que
# el factor contiene datos numéricos y el vector también los almacena como números

# Respuesta(c)
elem <- c(34, 0, 1, 23, 1, 2, 33, 1, 1, 42, 0, 1, 41, 0, 2)
A <- matrix(elem, ncol = 3, byrow = TRUE)

# Realiza la coerción de la matriz a un data frame
df <- as.data.frame(A)

# Como un data frame, coerce la segunda columna para que tenga un valor lógico
df[2] <- as.logical(df[2] == 1)
class(df$V2) # "logical"

# Como un data frame, realiza una coerción a la tercera columna para tener un valor de factor
df[3] <- as.factor(df[,3])
class(df$V3) # "factor"