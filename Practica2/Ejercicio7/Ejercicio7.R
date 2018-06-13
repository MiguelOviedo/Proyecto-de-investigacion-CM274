# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)

S <- 9 # tamaño de la muestra

Info <- sample(c("+", "-"), size = S, replace = TRUE)
Info # muestra original

A <- sample(c("+", "-", "?"), size = S, replace = TRUE)
A # muestra recibida

n <- length(A)
B <- 0 # total de caracteres válidos

if(n >= 1 || n <= 10){ # 1 <= |A| <= 10
  
  for(i in 1:n){
    caracter <- A[i]
    if(caracter != "?") # si el elemento es "+" o "-" es resultado válido
        B <- B + 1
  }

  p <- B/S # probabilidad
  p
  format(round(p,9), nsmall=9) # resultado con 9 decimales
}

# Respuesta(b)
suma_dado <- c(0,0)

for(i in 1:2){

  D1 <- sample(1:6, 1, replace = TRUE, prob = NULL)
  D2 <- sample(1:6, 1, replace = TRUE, prob = NULL)
  D3 <- sample(1:6, 1, replace = TRUE, prob = NULL)
  
  suma_dado[i] <- D1 + D2 + D3
}

B <- suma_dado[1] # resultado de Vilma
C <- suma_dado[2] # resultado de Esther

D <- 0 # total de casos favorables

# vector de frecuencias de lanzamiento de 3 dados
X <- c(NA, NA, 1, 3, 6, 10, 15, 21, 25, 27, 27, 25, 21, 15, 10, 6, 3, 1)

for(i in max(B,C):length(X))
  D <- D + X[i]

S <- 6*6*6 # total de resultados posibles

P <- D/S
P
paste(D,"/",S)

############# ALGORITMO PARA CALCULAR LA SUMA DE 3 DADOS #############

# Sea X una variable aleatoria de lanzar 3 dados y calcula la suma

X <- c(NA, NA, rep(0, 16)) # vector vacío que almacenará la frecuencia de cada lanzamiento
total <- 0 # contador del total de lanzamientos posibles
flag <- 0 # bandera

while(total != 216){
  
  # muestra de un lanzamiento de 3 dados
  D1 <- sample(1:6, 1, replace = TRUE, prob = NULL)
  D2 <- sample(1:6, 1, replace = TRUE, prob = NULL)
  D3 <- sample(1:6, 1, replace = TRUE, prob = NULL)
  
  if(total == 0){
    lista_ternas <- list(c(D1, D2, D3)) # para el primer lanzamiento
  }else{
    for(i in 1:length(lista_ternas))
      if(all(lista_ternas[[i]] == c(D1,D2,D3))){ # verifica que no haya ternas repetidas
        flag <- 1 # si se repite una terna, se descarta
        break # no es necesario hacer más iteraciones, el bucle termina
      }
  }
  
  if(flag == 0){ # no se repite la terna
    lista_ternas <- c(lista_ternas, list(c(D1, D2, D3))) # agregar a la lista de ternas
    suma_dado <- D1 + D2 + D3
    for(i in 3:18)
      if(suma_dado == i){
        X[i] <- X[i] + 1 # agrega la frecuencia de ocurrencia
        total <- total + 1 # como es una terna correcta, se suma al total
        break # no es necesario hacer más iteraciones, el bucle termina
      }
  }else{
    flag <- 0 # si flag es 1, entonces flag se vuelve a 0 para el siguiente lanzamiento
  }
  
} # while

for(i in 3:18)
  print(paste(i, "= ", X[i])) # imprime las frecuencias de lanzamiento de los 3 dados

sum(X[3:18]) # total de la muestra: 216

