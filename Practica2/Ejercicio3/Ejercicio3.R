# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
vec1 <- c(2,1,1,3,2,1,0)
vec2 <- c(3,8,2,2,0,0,0)

if((vec1[1]+vec2[2])==10){ cat("Imprime el resultado!") }
# vec1[1] = 2
# vec2[2] = 8
# vec1[1] + vec2[2] = 10
# Imprime el resultado

if(vec1[1]>=2&&vec2[1]>=2){ cat("Imprime el resultado!") }
# vec1[1] = 2 >= 2
# vec2[1] = 3 >= 2
# vec1[1]>=2&&vec2[1]>=2 es TRUE
# Imprime el resultado

if(all((vec2-vec1)[c(2,6)]<7)){ cat("Imprime el resultado!") }

# all() verifica si la operación de todos los elementos del vector es verdadera
# para vec1[2] - vec2[2] = 7, lo cual no es menor que 7
# No imprime el resultado

if(!is.na(vec2[3])){ cat("Imprime el resultado!") }
# is.na devuelve TRUE si hay un valor omitido (NA), en este caso es FALSE
# como is.na está negado la contición del if es TRUE
#Imprime el resultado

# Respuesta(b)
for(i in 1:length(vec1)){
  if(vec1[i] + vec2[i] > 3){
    print(paste("suma", vec1[i] * vec2[i]))
  }else{
    print(paste("producto", vec1[i] + vec2[i]))
  }
}

# Respuesta(c)

mymat <- matrix(as.character(1:16),4,4)

mymat <- matrix(c("DANDELION","Hyacinthus","Gerbera",
                  "MARIGOLD","geranium","ligularia",
                  "Pachysandra","SNAPDRAGON","GLADIOLUS"),3,3)

mymat <- matrix(c("GREAT","ejercicioss","agua","hey"),2,2,
                byrow=T)

# Código
d <- diag(mymat) # asignamos la diagonal de la matriz a una variable
flag <- FALSE # bandera, nos servirá para saber si las matrices tienen o no una cadena de caracter que empiece con 'g' o 'G'
for(i in 1:length(d)){
  # substr extrae un subcadena de la diagonal de mymatt de rango 1 hasta 1
  if(substr(d[i], 1, 1) == 'g' || substr(d[i], 1, 1) == 'G'){
    mymat[i,i] <- "AQUI"
    flag <- TRUE # la bandera cambia y la matriz mymat ya no será reemplazada por una matriz identidad
  }
}

# si la bandera no cambia, reemplazar mymat por una matriz identidad de misma dimensión
if (flag == FALSE){
  mymat <- diag(ncol(mymat))
}
mymat
