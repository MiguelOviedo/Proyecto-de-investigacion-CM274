# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)

minum <- 1
fact <- 1

if(minum == 0 || minum == 1){
  fact # para factorial de 0 y 1 el resultado es directo
}else{
  while(minum >= 2){
    fact <- fact * minum # calcula el factorial
    minum <- minum - 1 # decrementa contador
  }
  fact
}

# Respuesta(b)

unacadena <- "R fever"
index <- 1
ecount <- 0
resultado <- unacadena

while(ecount<2 && index<=nchar(unacadena)){
  caracter <- substr(unacadena, index, index) # extraemos cada caracter de la cadena
  if("e" == caracter || "E" == caracter)
    ecount <- ecount + 1 # si caracter es "e" o "E" sumamos 1 a ecount
  if(ecount == 2)
    resultado <- substr(unacadena, 1, index - 1) # si ecount es 2 extraemos la cadena hasta una posición anterior de la segunda "e" o "E"
    
  index <- index + 1
}
resultado