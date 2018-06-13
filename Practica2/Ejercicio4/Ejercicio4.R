# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)

loopvec1 <- 5:7
loopvec2 <- 9:6

mat1 <- matrix(loopvec1,length(loopvec1),length(loopvec2)) # en vez de NA, inicializamos la matriz con loopvec1
len <- length(loopvec1)*length(loopvec2) # longuitud total
loopvec2 <- rep(loopvec2, each=3) # modificamos loopvec2 de manera que cada elemento se repita 3 veces

for(i in 1:len)
  mat1[i] <- mat1[i]*loopvec2[i] # llena la matriz como si fuera un vector
mat1

# Respuesta(b)

nombres <- c("Peter","Homer","Lois","Stewie","Maggie","Bart") # cadena de prueba
numeros <- c() # inicializamos el vector a NULL

# el bucle concatena los resultados según hace las comparaciones con la cadena nombres
for(i in 1:6)
  numeros <- c(numeros, switch(EXPR=nombres[i],Homer=12,Marge=34,Bart=56,Lisa=78,Maggie=90, NA))

numeros # [1] NA 12 NA NA 90 56

# Respuesta(c)

unalista <- list(aa=c(3.4,1),bb=matrix(1:4,2,2),
                 cc=matrix(c(T,T,F,T,F,F),3,2),dd="cadena aqui", 
                 ee=list(c("hola","tu"),matrix(c("hola","there"))),
                 ff=matrix(c("red","green","blue","yellow")))

unalista <- list("salio algo raro",as.vector(matrix(1:6,3,2)))

unalista<- list(list(1,2,3),list(c(3,2),2),list(c(1,2),matrix(c(1,2))),
                rbind(1:10,100:91))

contador <- 0 # inicializa contador a 0

for(i in 1:length(unalista)){
  
  miembro_lista <- unalista[[i]] # cada elemento de unalista se almacena en miembro_lista para ser testeada
  
  if(is.matrix(miembro_lista))
     contador <- contador + 1 # si el elemento es de tipo matriz, sumar 1 al contador
     
  if(is.list(miembro_lista)) # entra aquí si es el elemento de la unalista es una lista
    for(i in 1:length(miembro_lista)) # itera sobre sus elementos
      if(is.matrix(miembro_lista[[i]]))
        contador <- contador + 1 # si el elemento es de tipo matriz, sumar 1 al contador
}
contador # total de cuenta
