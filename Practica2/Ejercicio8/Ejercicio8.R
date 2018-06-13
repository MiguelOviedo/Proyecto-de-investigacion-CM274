# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)

# probar solo con una
x <- list(1:4, matrix(1:4,1,4), matrix(1:4,4,1), matrix(1:4,2,2))
x <- list(diag(9),matrix(c(0.2,0.4,0.2,0.1,0.1,0.2),3,3),
          rbind(c(5,5,1,2),c(2,2,1,8),c(6,1,5,5),c(1,0,2,0)),
          matrix(1:6,2,3),cbind(c(3,5),c(6,5)),as.vector(diag(2)))
x <- "Hola" # imprimirá "Error, no es una lista", y se detendrá la ejecución
x <- list() # imprimirá "Error, la lista no tiene elementos", y se detendrá la ejecución

invertirMatriz <- function(x){
  
  # intercambiar asignaciones según se requiera
  noninv <- NA #Inf #matriz inadecuada
  nonmat <- "no es una matriz!" #666
  silent <- TRUE #FALSE
  
  # verifica si x es una lista
  if(!is.list(x)){
    print("Error, no es una lista")
    break
  }
  
  # verifica si la lista está vacía
  if(length(x) == 0){
    print("Error, la lista no tiene elementos")
    break
  }
  
  # verifica si nonmat es un cadena de caracteres
  if(typeof(nonmat) != "character"){
    
    print("nonmat no es una cadena de caracteres. Intentando coerzar ... ")
    nonmat <- as.character(nonmat) # coerce nonmat a cadena de caracteres
    
    # verifica si nonmat se pudo coerzar a cadena de caracteres
    if(typeof(nonmat) == "character"){
      print("Se coerzó nonmat a cadena de caracteres")
    }else{
      print("Error, no se pudo coerzar nonmat a cadena de caracteres")
      break
    }
    
  }
  
  for(i in 1:length(x)){
    
    if(is.matrix(x[[i]])){ # verifica si un elemento de la lista es una matriz
      
      mensaje <- try(solve(x[[i]]), silent) # try atrapa algún mensaje de error si lo hubiese
                                            # mensaje atrapa el mensaje de error cuando silent es TRUE
      if(class(mensaje) == "try-error") # verifica si hay errores
        x[[i]] <- noninv # si no se puede invertir, se reemplaza la matriz por el valor de noninv
      else
        x[[i]] <- solve(x[[i]]) # si se puede invertir, se reemplaza la matriz por su inversa
      
    }else{
      x[[i]] <- nonmat # si un elemento de la lista no es una matriz lo reemplaza por una cadena de caracteres
    }
  
  }
  
  return(x) # retorna la nueva lista
  
}
x <- invertirMatriz(x) # reemplaza la nueva lista
print(x) # imprime la nueva lista
