#Author: Daniel Hidalgo
#Title: Find the lady - A.K.A.: Three-card Monte
#Hacemos por pasos, definimos que existen 3 cartas donde 1 de ella es la reina de corazones y las demas jocker
my.card <- c(1:3)
my.card[1] <- "Queen of hearts"
my.card[2] <- "Jocker"
my.card[3] <- "Jocker"

#Consideramos que empezamos y queremos saber si ganaremos o perderemos dinero
my.money <- 0


#Primero, reacomodamos las cartas de forma aleatoria
my.card <- sample(my.card)

#Escogemos una carta
my.pick <- readline(prompt = "Elige una carta, de izquierda a derecha: ¿1, 2 o 3?: ")
while(as.integer(my.pick) > 3 || as.integer(my.pick) < 1){
  my.pick <- readline(prompt = "Solo puedes elegir entre el 1 y el 3: ")
}

#Ganamos dos (2) dolares si acertamos y en caso contrario perdemos un (1) dolar
if(my.card[as.integer(my.pick)] == "Queen of hearts"){
  my.money <- my.money + 2
} else{
  my.money <- my.money - 1
}

#Ahora hagemos lo mismo pero en el caso de que se juegue hasta que ganemos por primera vez
#Cantidad de veces jugadas
my.plays <- 0
my.money <- 0
win <- FALSE
while(win != TRUE){
  my.card <- sample(my.card)
  my.pick <- sample(1:3,1)
  
  if(my.card[as.integer(my.pick)] == "Queen of hearts"){
    my.money <- my.money + 2
    my.plays <- my.plays +1
    win <- TRUE
  } else{
    my.money <- my.money - 1
    my.plays <- my.plays +1
  }
}

print(paste("Dinero obtenido: ", my.money, " Veces jugadas:", my.plays))


#Modulamos nuestro código para jugar sin importar si ganamos o perdemos, y veamos el resultado
my.plays <- 0
my.money <- 0

#Digamos que jugamos 1000 veces
while(my.plays < 1001){
  my.card <- sample(my.card)
  my.pick <- sample(1:3,1)
  
  if(my.card[as.integer(my.pick)] == "Queen of hearts"){
    my.money <- my.money + 2
    my.plays <- my.plays +1
  } else{
    my.money <- my.money - 1
    my.plays <- my.plays +1
  }
}

my.expected[4] <- my.money
print(paste("Dinero obtenido: ", my.money, " Veces jugadas:", my.plays))

#Ahora tomaremos muestras de mil (1000) jugadas y las evaluaremos dentro de un array, la media y la desviación estandar
my.expected <- c(1:1000)
numExpect <- 0
my.plays <- 0
my.money <- 0
while(numExpect < 1001){
  while(my.plays < 1001){
    my.card <- sample(my.card)
    my.pick <- sample(1:3,1)
    
    if(my.card[as.integer(my.pick)] == "Queen of hearts"){
      my.money <- my.money + 2
      my.plays <- my.plays +1
    } else{
      my.money <- my.money - 1
      my.plays <- my.plays +1
    }
  }
  my.expected[numExpect] <- my.money
  my.plays <- 0
  numExpect <- numExpect + 1
}

my.expected
mean(my.expected)
sd(my.expected)
