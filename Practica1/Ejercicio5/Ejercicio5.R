# Nombre : Miguel Angel Oviedo Rodriguez 20131463I
# Respuesta5
# Descrición del código

plot.new()  # Crea un nuevo gráfico, debe llamarse primero para poder definir todos los datos
plot.window(range(pressure$temperature),
            range(pressure$pressure)) # range() define el rango de valores para los ejes X e Y
box()   # Dibuja un marco
axis(1) # Dibuja el eje X a partir del rango definido en plot.window. El argumento 1 dibuja el eje en la línea inferior y horizontal con respecto al marco
axis(2) # Dibuja el eje Y a partir del rango definido en plot.window. El argumento 2 dibuja el eje en la línea izquierda y vertical con respecto al marco
points(pressure$temperature, pressure$pressure) # Dibuja los puntos según las coordenadas
mtext("temperatura", side=1, line=3) # Etiqueta el eje X con un margen de 3 líneas en el lado 1 (eje X)
mtext("presion", side=2, line=3) # Etiqueta el eje Y con un margen de 3 líneas en el lado 2 (eje Y)
mtext("Presion de vapor de Mercurio\ncomo una funcion de la Temperatura",
      side=3, line=1, font=2) # Etiqueta el título con un margen de 1 línea con fuente 2

# Versión con Grid
library(grid) # cargamos la librería grid para poder usar las funciones asociadas
pushViewport(viewport(layout=grid.layout(1, 1), gp=gpar(cex=0.9))) # grid.layout divide el viewport en regiones de una fila y una columna. gp es un objeto de la clase gpar que lista un conjunto de parámetros como cex que establece el tamaño de letra especificada.
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1)) # El grafico ocupa toda la fila 1 y columna 1, es decir en este caso, todo el gráfico
pushViewport(plotViewport(c(5, 4, 5, 2))) # Dimensiones del gráfico respecto al marco
pushViewport(dataViewport(xData=pressure$temperature, yData=pressure$pressure)) # Usa los datos de temperatura y presión que serán graficados en los ejex X e Y
grid.rect() # Dibuja un rectangulo
grid.xaxis(at=c(0,50,100,150,200,250,300,350)) # Dibuja el eje X con las marcas de los elementos del vector
grid.yaxis(name="Y", at=c(0,200,400,600,800)) # Dibuja el eje Y con las marcas de los elementos del vector y le damos un nombre "Y"
grid.edit(gPath("Y","labels"), rot=90) # Para rotar las marcas 90 grados usamos un hijo de yaxis, labels, mediante gPath. 
grid.text("temperatura", y=unit(-3, "lines")) # Escribe "temperatura" a una distancia de 3 líneas del origen hacia el eje -Y
grid.text("presion", x=unit(-3, "lines"), rot=90) # Escribe "presion" a una distancia de 3 líneas del origen hacia el eje -X con una rotación  del texto de 90°
grid.text("Presion de vapor de Mercurio\ncomo una funcion de la Temperatura",
          x=unit(180, "native"), y=unit(950, "native"),gp=gpar(fontface = "bold")) # Texto principal a una distancia de 180x950 relativa al gráfico y en negrita
grid.points(pressure$temperature, pressure$pressure,gp=gpar(cex=0.7)) # dibuja los puntos de presion vs temperatura, aquí cex que establece el tamaño del tipo de objeto dibujado

