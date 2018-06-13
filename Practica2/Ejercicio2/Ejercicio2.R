# Nombre : Miguel Angel Oviedo Rodriguez 20131463I

# Respuesta2

# Argumentos exactos : especifican los atributos de una función explícitamente y escritos de forma completa
seq(from=10,to=8,length=5)
sort(decreasing=T,x=c(2,1,1,2,0.3,3,1.3))

#Argumentos parciales: se pueden especificar atributos de forma abreviada teniendo cuidado de que no haya otros atributos con la misma abreviación
which(matrix(c(T,F,T,T),2,2),a=T) # a = arr.ind

# Argumento posicional: se declaran atributos de manera implícita respetando el orden en que son definidos
rep(1:2,3)
which(matrix(c(T,F,T,T),2,2))

# Argumento Mixto: uso de argumentos exactos y mixtos en un misma función
array(8:1,dim=c(2,2,2)) # dim = dimension