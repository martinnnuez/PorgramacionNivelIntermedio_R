#Discriminacion parametrica
setwd("C:/Users/Jose Maria/Desktop/TALLER DE SOFTWARE 2/segunda semana")
#Todos en C:\Users\Jose Maria\Desktop\TALLER DE SOFTWARE 2\segunda semana
#C es la firma espectral y cobertura la respuesta
#aca no tengo la respuesta
#suele ocurrir que el que quiero clasificar sea mas grande que el de entrenamiento
#por suerte todas las clases estan bastante balanceadas, si esta subrepresentada tipicamente no termiamos viendola


#vamos a hacer la funcion
#x=una de las filas del archivo sin clasificar
#datos= donde estan clasificados
#k= cantidad de vecinos si no especifico usa 3
#calcular la distancia de x y cada una de las filas de datos
#ordenar las distancias

#seleccionar las k distancias menores

#averiguar la frecuencia de los labels, rotulos de las k distancias mas pequeñas

#recortar como clase el rotulo mas frecuente

#Discriminacion no parametrica
Datos=read.table("Entrenamiento.txt",header = T,sep="\t")
dim(Datos)
head(Datos)
DatosNC=read.table("noclasificados.txt",header = T,sep="\t")
dim(DatosNC)
head(DatosNC)
table(Datos$Cobertura)
k=3
#x es el valor desconocido que quiero clasificar, aqui la base de datos Datos participa de la clasificacion
# porque en cada corrida comparo las ditancias

clasificador=function(x,Datos,k=3) #recibe estos parametros x,Datos,k=3
{
  #calcular distancia entre x y cada uno de las filas de Datos
  distancias=apply(Datos[,-1],1,function(fila) #apply hace como un for
  {
    dist(rbind(x,fila)) #agarra cada fila y calcula la distancia contra esos valores. Se obtiene un valor de distancia entre ambos vectores
  })
  #devuelve un vector por ser apply con las distancias, y saber que le pasa la fila=1 con esos valores, porque parte de esos datos y no el valor 1.
  #va calculando las distancias entre x y todas las filas
  names(distancias)=Datos[,1] #a cada uno de los valores de distancia le asigna el valor de la clasificacion
  tabla=table(names(sort(distancias)[1:k]))#aca le pido una tabla de las distancias ordenadas de menor a mayor y que tome las primeras 3 [1:k]
  names(tabla[which.max(tabla)])#y que me devuelva el nombre del cual hay mas repeticiones
} 

#calcula la distancia
dist(rbind(9,f))
f<-c(1,2,3)
t<-c(4,5,6)

m<-matrix(1:10,nrow=2) #esto lo que queria
m<-data.frame(1:10,nrow=2)# aca se hiso una col 1:10 y otra con 2 cte #bueno saberlo
?data.frame()
#apply automaticamente guarda los valores en un vector
apply(m,2,mean)

sort(c(4,6,7,3,2)) #[1] 2 3 4 6 7
names(tabla[which.max(tabla)])
?names()#le pone los nombres a distancias

apply(Datos[,-1],1,function(fila) #apply hace como un for
{
  fila #aca puedo ver que en fila se guarda los valores de cada una de las filas en cada corrida,
  #va recorriendo por fila, y teniendo todos los datos en cada fila
})

#al k hace falta correrlo globalmente y que se guarde
a=clasificador(c(130,70,58,129),Datos)
a

#usamos apply y recorra por filas
#y ponemos que funcion usar
#el primer argumento se lo pasa directo a apply
#no hace falta darle el argumento

pred=apply(DatosNC,1,function(fila){clasificador(fila,Datos,k=3)})
#escribimos de forma explicita
#en pred guardo el nombre de las clases

?dist()

DatosNCP=cbind(DatosNC,pred)
head(DatosNCP)

table(pred)/sum(table(pred))*100

pred=apply(Datos[,-1],1,function(fila){clasificador(fila,Datos,k=1)})
DatosReclasificados=cbind(as.character(Datos$Cobertura),pred)
#confusion matrix
table(DatosReclasificados[,1],DatosReclasificados[,2])
#clasifica todos bien con k=1


#vamos a hacer un crossvalidation
pred=sapply(seq(1:nrow(Datos)),function(i)
{
  clasificador(Datos[i,-1],Datos[-i,],k=10)#x o valor desconocido, datos y k
})

#function i hace que vaya desde 1 hasta el numero de nrow datos
#ese i varia de 1 a 72

#voy eliminando uno a la vez y voy reclasificando, tecnica de validacion por validacion cruzada

DatosReclasificados=cbind(as.character(Datos$Cobertura),pred)
#confusion matrix
table(DatosReclasificados[,1],DatosReclasificados[,2])
#tabla mas confiable que la primera porque es una de reclasificacion


##ejemplito que me dio de lo que hace el apply y como modificarlo
sapply(Datos$Cobertura,function(x) sub("Suelo","area",x))
sub("c","C","carlos")


#ultima parte
TCC=table(DatosReclasificados[,1],DatosReclasificados[,2])

tasasError=sapply(3:8,function(k){
  pred=sapply(seq(1:nrow(Datos)),function(i)
  {
    clasificador(Datos[i,-1],Datos[-i,],k)
  })
  DatosReclasificados=cbind(as.character(Datos$Cobertura),pred)
  TCC=table(DatosReclasificados[,1],DatosReclasificados[,2])
  total=sum(TCC)
  diag(TCC)=0
  sum(TCC/total)
})

plot(3:8,tasasError,type="l")
