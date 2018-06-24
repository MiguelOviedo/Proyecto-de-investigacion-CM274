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
#my.pick <- readline(prompt = "Elige una carta, de izquierda a derecha: ¿1, 2 o 3?: ")
my.pick <- sample(1:3,1)
while(as.integer(my.pick) > 3 || as.integer(my.pick) < 1){
  my.pick <- readline(prompt = "Solo puedes elegir entre el 1 y el 3: ")
}

#Ganamos dos (2) dolares si acertamos y en caso contrario perdemos un (1) dolar
if(my.card[as.integer(my.pick)] == "Queen of hearts"){
  my.money <- my.money + 2
} else{
  my.money <- my.money - 1
}

print(paste("Dinero obtenido: ", my.money))

#Ahora hacemos lo mismo pero en el caso de que se juegue hasta que ganemos por primera vez (Las vegas algorithm - Algoritmo de Las Vegas: Nos asegura un resultado satisfactorio)
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


#Modulamos nuestro código para jugar una cantidad veces como máximo así no ganemos, y veamos el resultado (Monte Carlo algorithm - Algoritmo de Monte Carlo: En un tiempo finito buscamos la respuesta que deseamos)
my.plays <- 0
my.money <- 0
win <- FALSE

#Digamos que jugamos 5 veces como máximo hasta ganar 1 vez
while(my.plays < 5 && win == FALSE){
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


#En un caso especial, digamos que queremos obtener 50 victorias.
#Veamos cuántas jugadas podría tomar con el algoritmo de Las Vegas
my.plays <- 0
my.money <- 0
my.wins <- 0

while(my.wins < 500){
  my.card <- sample(my.card)
  my.pick <- sample(1:3,1)
  
  if(my.card[as.integer(my.pick)] == "Queen of hearts"){
    my.money <- my.money + 2
    my.plays <- my.plays + 1
    my.wins <- my.wins + 1
  } else{
    my.money <- my.money - 1
    my.plays <- my.plays + 1
  }
}

print(paste("Dinero obtenido: ", my.money, " Veces jugadas:", my.plays))


#Veamos qué pasa si queremos repetir dicha hazaña: 100 veces 500 victorias
my.plays <- 0
my.money <- 0
my.wins <- 0
my.gamesplayed <- c(1:100)
my.expectedMoney <- c(1:100)
rounds <- 1
while(rounds < 101){
  while(my.wins < 500){
    my.card <- sample(my.card)
    my.pick <- sample(1:3,1)
    
    if(my.card[as.integer(my.pick)] == "Queen of hearts"){
      my.money <- my.money + 2
      my.plays <- my.plays + 1
      my.wins <- my.wins + 1
    } else{
      my.money <- my.money - 1
      my.plays <- my.plays + 1
    }
  }
  my.gamesplayed[rounds] <- my.plays
  my.expectedMoney[rounds] <- my.money
  my.money <- 0
  my.plays <- 0
  my.wins <- 0
  rounds <- rounds + 1
}

print("Jugadas:")
my.gamesplayed
print("Dinero:")
my.expectedMoney
print(paste("Media: ", mean(my.expectedMoney), " Desviación estandar: ", sd(my.expectedMoney)))
x <- data.frame("Partidas" = my.gamesplayed, "Dinero" = my.expectedMoney)         
with(x,plot(Partidas,Dinero))

#Ahora usaremos Monte Carlo para tratar de conseguir: 100 veces 500 victorias con un máximo de 1500 jugadas por vez
my.plays <- 0
my.money <- 0
my.wins <- 0
my.gamesplayed <- c(1:100)
my.expectedMoney <- c(1:100)
my.victory <- c(1:100)
rounds <- 1
my.victories <- 0
while(rounds < 101){
  while(my.wins < 500 && my.plays < 1500){
    my.card <- sample(my.card)
    my.pick <- sample(1:3,1)
    
    if(my.card[as.integer(my.pick)] == "Queen of hearts"){
      my.money <- my.money + 2
      my.plays <- my.plays + 1
      my.wins <- my.wins + 1
    } else{
      my.money <- my.money - 1
      my.plays <- my.plays + 1
    }
  }
  my.gamesplayed[rounds] <- my.plays
  my.expectedMoney[rounds] <- my.money
  
  if(my.wins == 500){
    my.victory[rounds] <- "Win"
    my.victories <- my.victories + 1
  }
  else{
    my.victory[rounds] <- "Lose"
  }
  
  my.money <- 0
  my.plays <- 0
  my.wins <- 0
  rounds <- rounds + 1
}

print("Jugadas:")
my.gamesplayed
print("Dinero:")
my.expectedMoney
print("Gano?:")
my.victory
print(paste("Ganó unas: ", my.victories, " veces"))
print(paste("Media: ", mean(my.expectedMoney), " Desviación estandar: ", sd(my.expectedMoney)))

x <- data.frame("Partidas" = my.gamesplayed, "Dinero" = my.expectedMoney)
with(x,plot(Partidas,Dinero))
