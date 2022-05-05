Y=c(3,2,5,6,8,6)
trat=c("A","A","B","B","C","C")
miModelo=lm(Y~trat)

#Modelo que tiene ordenada al origen y que tiene efecto de los tratamientos
#Si queremos ponerla 1+, si no la queremos -1+ y la saca

miModelo$coefficients
#deberia esperar 4 coeficientes, me elimino trat A.
#como consecuencia de que es el que elimina por la reparametrizacion.
#CATEGORIA DE REFERNCIA CUANDO TRABAJAMOS CON VARIABLES DUMMMY.

model.matrix(miModelo)
#matriz de incidencia, no tenemos tratA como era de esperar

#Como interptreto los coeficientes:
#medias de los tratamientos, m+ t1; m+t2; m+t3

#media tratamiento A
1 0 0
#media tratamiento B
1 1 0
#media tratamiento C
1 0 1

M=matrix(c(1,0,0,1,1,0,1,0,1),byrow=T,ncol=length(miModelo$coefficients))
M
#esta matriz aplicada al vector de coeficientes me tiene que devolver las medias

#M*BETA daria las medias de uno de los tratamientos
M%*%miModelo$coefficients
#que significa entonces cada uno de los ceoficientes
#m corresponde a la media del tratA
#Mientras que tratB y tratC son las diferencias entre la media del tratA y la de los otros tratamientos

#cambia el A a zA, cambian coeficientes, pero las medias siempre iguales

anova(miModelo)
#devuelve p valor 0.04419

#como genero eso a mano
#Vamos a hacer anova con la expresion esa del W
#test de hipotesis sobre las medias
#ESTO PREGUNTA EN EXAMENES
#COMO HARIA H PARA ESTE CASO

#k y h es k*m
K=matrix(c(1,-1,0,1,0,-1),ncol=3,byrow=T)
#h chiquita es 00, puedo omitirla en la expresion de W
#no la escribo

H=K%*%M

#matriz de covarianzas HB
mivcov=H%*%vcov(miModel)%*%t(H)
#ese vcov(miModelo) es la matriz de covarianzas de los beta estimados 

#coeficientes modelo
b=miModelo$coefficients

W=(t(H%*%b)%*%solve(mivcov)%*%(H%*%b))/nrow(H)
#da 10.5 lo mismo de cuadrito de anava, perfecto

#como calculamos el p valor de esta f
#quiero el pvalor de esa f
1-pf(W,nrow(H),length(Y)-length(b))
#aca esta el pvalor que buscabamos

#De donde sale la suma de cuadrados del tratamiento
X=model.matrix(miModelo)
#matriz de proyeccion P
P=X%*%solve(t(X)%*%X)%*%t(X)

I=diag(1,nrow(X))
#R seria la parte residual
R=I-P

#suma de cuadrados residual
t(Y)%*%R%*%Y
#da 3 fantastico

#lo mismo con P en vez de R
t(Y)%*%P%*%Y
#da 171, no esta corregida.

#como la corrijo

U=matrix(rep(1,nrow(X)),ncol=1)
P1=U%*%solve(t(U)%*%U)%*%t(U)

#Esta es la corregida
t(Y)%*%(P-P1)%*%Y
#y nos da el 21 que estabamos esperando

########PROBAR PROMEDIO (A,B) VS C
#el hace con K estas cosas K*M igual H
#le gusta 1/2 porque puede ver diferencia

K=matrix(c(1/2,1/2,-1),byrow=T,ncol=3)
H=K%*%M
mivcov=H%*%vcov(miModelo)%*%t(H)
W=(t(H%*%b)%*%solve(mivcov)%*%(H%*%b))/nrow(H)
1-pf(W,nrow(H),length(Y)-length(b))

#cuanto vale la diferencia estimada
EDif=H%*%b
#-3 perfecto
#TIENE DISTRIBUCION NORMAL CON MEDIA Y VARIANZA POR SER COMBINACION LINEAL
#ESTO ME ABRE LA PUERTA PARA CALCULAR EL ERROR ESTANDAR Y DE AHI EL INTERVALO DE CONFIANZA

#cual es el error estandar
#todo sale de mi cov
EE=sqrt(H%*%vcov(miModelo)%*%t(H))

#calculo intervalo de confianza para alfa 0.05 estimacion +- t * el error estandar de ese estadistico
gle=length(Y)-length(b)
LI=EDif-qt(0.975,gle)*EE
LS=EDif+qt(0.975,gle)*EE

c(LI,LS)





###Forma resolver imponiendo restriccion
X=model.matrix(Y~trat-1)
X=cbind(U=1,X)
XPX=t(X)%*%X
solve(XPX)
C=matrix(c(0,1,1,1),ncol=4)
t(X)%*%X

XPXConRestriccion=rbind(cbind( XPX , t(C) ),cbind(C,0))

solve(XPXConRestriccion)%*%c(t(X)%*%Y,0)

solve(rbind(cbind(XPX,t(C)),cbind(C,0)))%*%c(t(X)%*%Y,0)
X=rbind(X,C)
Y=c(Y,0)
XPX=t(X)%*%X
solve(XPX)%*%t(X)%*%Y
