# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta(a)
sexo <- c("F", "M", "M", "M", "F", "F", "F", "M", "M", "M", "M", "F", "M", "F", "F", "F", "M", "M", "M", "M")

partido <- c("Laborista","Nacional","Nacional", "Laborista","Nacional", "Verdes","Nacional","Nacional", "Verdes", "Otros",
             "Verdes", "Laborista","Nacional","Nacional", "Laborista", "Laborista","Nacional","Nacional", "Laborista", "Otros")

# Respuesta(b)
# En este caso no tiene sentido usar ordered ya que los elementos de cada vector
# no tienen un sentido jerárquico sino nominal
# En caso de un factor de caracteres el nivel se da según el orden alfabético por defecto,
# o según se le indique de forma explícita
f_sexo <- factor(sexo)
f_partido <- factor(partido)

# # Respuesta(c)
partido_hom <- f_partido[c(2:4, 8:11, 13, 17:20)]
generos_nac <- f_sexo[c(2, 3, 5, 7, 8, 13, 14, 17, 18)]

# Respuesta(d)
# OBS: Hay 25 resultados, no 26
f_partido <- factor(c(partido, "Nacional", "Maori", "Maori", "Laborista",  "Verdes", "Laborista"))
f_sexo <- factor(c(sexo, "M", "M", "F", "F", "F", "M"))

confianza <- c("Alto", "Moderado", "Bajo", "Alto", "Moderado", "Alto", "Moderado", "Bajo",
               "Moderado",  "Moderado", "Moderado", "Moderado", "Moderado", "Moderado", "Moderado",
               "Moderado", "Moderado", "Moderado", "Moderado", "Bajo", "Bajo", "Moderado", "Alto", "Bajo", "Moderado")
res <- factor(confianza, order = TRUE, levels = c("Bajo", "Moderado", "Alto"))
res_lab <- res[c(1, 4, 12, 15, 16, 19, 24, 25)]
res_nac <- res[c(2, 3, 5, 7, 8, 13, 14, 17, 18, 21)]

# Observando los resultados se concluye que el nivel de confianza para el partido Laborista es mayor que el Nacional
