# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
personas <- c("Stan", "Francine", "Steve", "Roger", "Hayley", "Klaus")
sexo <- factor(c("M", "F", "M", "M","F", "M"), levels = c("F", "M"))
puntuacion <- factor(c("Alto", "Medio", "Bajo", "Alto", "Medio", "Medio"), levels = c("Bajo", "Medio", "Alto"), ordered = TRUE)

dframe <- data.frame(personas, sexo, puntuacion)

# Respuesta(b)
# Podemos agregar otra columna al dataframe con la función cbind
edad <- c(41, 41, 15, 1600, 21, 60)
dframe <- cbind(dframe, edad)

# Respuesta(c)
# Cambiamos los índices de las columnas de dframe según lo pedido
dframe <- dframe[,c(1,4,2,3)]

# Respuesta(d)
# El data frame misdatos es: 
misdatos <- data.frame(personas=c("Peter","Lois","Meg","Chris","Stewie","Brian"),
                       edad=c(42,40,17,14,1,7),
                       sexo=factor(c("M","F","F","M","M","M")),
                       puntuacion=c("Alto","Alto","Bajo","Medio","Alto","Medio"))
misdatos$edad.mon <- misdatos$edad*12
misdatos

# Eliminamos la última columna:
misdatos2 <- misdatos[,-5]
misdatos2

# Respuesta(e)
# Como los dos frames tienes la mismas características podemos combinarlos con rbind:
misdatosframe <- rbind(misdatos2, dframe)
misdatosframe