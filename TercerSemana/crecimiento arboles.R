#EJMEPLO PROYECTO 8
rm(list=ls())
setwd("C:/Users/Jose Maria/Desktop/TALLER DE SOFTWARE 2/DATOS")
datos<-read.delim("Competencia.txt")
datos

table(datos$Region)#3 regiones
tapply(datos$Crecimiento,datos$Region,mean,na.rm=T)
#se ve que en chaco arido menor crecimiento
#ahora anlaizis

modelo=lm(Crecimiento~Region,data=datos)
anova(modelo)
#p valor  0.116 no detecta diferencias 

#alguien midio las distancias a las cuales aparecen los arboles vecinos
#A1,A2,A3,A4...
#Existe las sospecha de que el crecimiento esta condicionado por la cantidad de vecinos contra los cuales esta compitiendo

head(datos)

#voy a hacer una funcion que devuelva AIC, le llamo 
miaic=function(radio,datos)
{
  #radio=3     para imaginarme como contar numero de arboles
  #tengo que recorrer a matriz de datos apply y quiero que sea fila por fila 1
  #recordar que le estoy pasando toda la fila entonces eliminar las primeras dos columnas, me pasa esa tirita sin esas dos columnas
  #cada vez que se ejecute me pasa esa tirita y esa tirita se llama x
  datos$n=apply(datos[,-c(1,2)],1,function(x){ sum(x<=radio)}) #en n se almacena eso, agregando columna a los datos
  modelo=lm(Crecimiento~Region+n+Region*n,data=datos)
  AIC(modelo)

}

sum(c(T,T,T,T,F,F,T))
#Transforma T=1 Y F=0 asique ese apply devuelve cuantos arboles tiene cerca de determinado radio
nrow(datos)#tenemos 39 datos, devuelve vector con 39 numeros el apply

#Ahora corro la funcion en un sapply
radios=seq(0.5,20,0.1)#todos los valores de radio que quiero que pruebe

aics<-sapply(radios,miaic,datos)#sapply vector de salida
#voy moviendo los radios muy poquito cada vez
#recibe como primer argumento directamente el radio que va variando
#sapply sobre entiende que le pasa el primer argumento al primer parametro de la funcion y luego defino datos como el otro

# esta funicon me va a dar un AIC para cada radio que estoy definiendo en la secuencia

#luego de correr el sapply los datos permanecen como antes

aics
#ahora analizo como cambian esos valores

plot(radios,aics,type="l")
abline(v=3.8,col="red",lty=3)

#para saber donde esta esa caida busco el minimo de aic

which.min(aics)
#posisicion 34

radios[which.min(aics)]
#en 3.8 metros se alcanza el minimo AIC

# los datos no tienen la n pq se modifcaron dentro de la funcion
# ahora tenemos el limite hasta el cual sirve la covariable que es 3.8 y luego se ensucia

radio=3.8
datos$n=apply(datos[,-c(1,2)],1,function(x){ sum(x<=radio)}) #en n se almacena eso, agregando columna a los datos
modelo=gls(Crecimiento~Region+n+Region*n,data=datos)
#ahora n teine num de arboles en entorno de 3.8 metros
#corro modelo 

anova(modelo)
?contr.sum
#hago truco del gls
datos$Region=C(datos$Region,contr.sum)
#C mayuscula
library(nlme)

anova(modelo)

#veo model matrix sin truquito
model.matrix(modelo,datos)
#region arida la elimino y productos de covariables por dummys 

#hago truco, vuelvo a correr y pido model.matrix es una forma de reparametrizar 

anova(modelo, type="marginal")

#dan distintos los coeficientes 
# ese truco excelente tener enc uenta


#hiso el profe 
rm(list=ls())
Datos=read.delim("Competencia.txt")
head(Datos)
table(Datos$Region)
tapply(Datos$Crecimiento,Datos$Region,mean,na.rm=T)
Modelo=lm(Crecimiento~Region,data=Datos)
anova(Modelo)

miAIC=function(radio,Datos)
{
  #radio=3
  Datos$n=apply(Datos[,-c(1,2)],1,function(x) sum(x<=radio))
  modelo=lm(Crecimiento~Region+n+Region*n,data=Datos)
  AIC(modelo)
}
radios=seq(0.5,8,0.1)
length(radios)
AICs=sapply(radios,miAIC,Datos)

plot(radios,AICs,type="l")
radios[which.min(AICs)]
abline(v=3.8,col="red",lty=3)

radio=3.8
Datos$n=apply(Datos[,-c(1,2)],1,function(x) sum(x<=radio))
Datos$Region=C(Datos$Region,contr.sum)
library(nlme)
modelo=gls(Crecimiento~Region+n,data=Datos)
anova(modelo,type="marginal")
model.matrix(modelo,Datos)
