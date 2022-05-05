#pongo semilla aleatoria para que esa generacion de la normal haga lo mismo para todos.
x=seq(1,20)
set.seed(5)
n=length(x)
Y= 30+0.7*x+rnorm(n,0,1)
plot(x,Y)
modelo=lm(Y~x)
modelo
sigma(modelo) #estimador de la raiz cuadrada de la varianza residual
#Vamos a intentar reproducir resultados
#construir matriz del modelo

model.matrix(modelo)

###Ajustar el modelo matricialmente
X=cbind(1,x)
X
dim(X)

#estimador maximo verosimil del beta
b=solve(t(X)%*%X)%*%t(X)%*%Y
b

plot(x,Y);abline(modelo)
plot(x,Y);abline(b)

dim(X)
dim(Y)

#vector proyectado en el plano
PX= X%*%solve(t(X)%*%X)%*%t(X)
    
PX%*%Y #proyeccion en el plano

I=diag(n)
(I-PX)%*%Y #PROYECCION ORTOCOMPLEMENTARIA

cbind(Y,(I-PX)%*%Y+PX%*%Y) #vuelvo al vector de donde comence 
#la suma da el y


(I-PX)%*%Y
resid(modelo)
cbind((I-PX)%*%Y,resid(modelo))
#son los mismos

#calculamos suma de residuos al cuadrado
t((I-PX)%*%Y)%*%((I-PX)%*%Y)
#esto se puede simplificar

t(Y)%*%(I-PX)%*%Y
#suma de los cuadraros de los residuos

#sigma:
t(Y)%*%(I-PX)%*%Y/sum(diag(I-PX))
sum(diag(I-PX))#traza de (I-PX)

sigma(modelo)^2
#dan lo mismo perfecto



####################3
mibeta=modelo$coefficients
mibeta
micov=vcov(modelo)
micov


plot(x,Y);abline(b);abline(h=mean(Y),v=mean(x))
#la recta de regresion para por el punto donde se juntan las medias

#errores estandares
EEbeta=sqrt(diag(micov))

cbind(mibeta,EEbeta)

miT=mibeta/EEbeta
miT#valor del estadistico t para la prueba de hipotesis de que bo b1 son ceros

pvalue=(1-pt(miT,sum(diag(I-PX))))*2
pvalue

cbind(mibeta,EEbeta,miT,pvalue)

miSummary=summary(modelo)
miSummary$coefficients
#son lomismo!!!!!  

#######queremos predecir un valor particular
predict(modelo,newdata=data.frame(x=5.5))

####################################
#23/09/2020

vcov(modelo)

solve(t(X)%*%X)*(sigma(modelo)^2)
#poner X mayuscula, da la matriz de los beta

HAT= X%*%solve(t(X)%*%X)%*%t(X)
HAT

HAT%*%Y

covHat=HAT*sigma(modelo)^2 #covarianza de valores esperados
covHat

dim(covHat)
#leverage cada uno de los elementos diagonales de esta matriz
#p es el numero de columnas de x

#si el leverage es mayor que lo siguiente es un leverage grande
sum(diag(HAT))*2/n# esto es 
#da 0,2

plot(seq(20),diag(HAT), ylim =c(0,0.25))
abline(h=0.2)
#vemos que ningun leverage supera esa linea
#por lo tanto no debemos preocuparnos por leverage

plot(x,Y);abline(b, col="red")
#construir intervalo de confianza para cada valor distinto de x
#para cada valor de x dos puntitos que limitan el intervalo de confianza para x
#como se construye, el intervalo de confianza de la media de y para un valor de x
#calcular el error estandar de y y multiplcarlo por el valor correspondiente.
# y esa varianza sale de covHat
#es la matriz de varianza y covarianza de los y sombrero
#los elementos diagonales son las varianzas de los valores esperados

diag(covHat)
#estas son las varianzas de y sombero

sqrt(diag(covHat))
#sigmas
YHAT=HAT%*%Y

LI=YHAT-2*sqrt(diag(covHat))
LS=YHAT+2*sqrt(diag(covHat))

#AHORA PLOTEO Y AGREGO LOS LIMITES
plot(x,Y);abline(b, col="red")
points(x,LI,type="l", col="blue")
points(x,LS,type="l", col="blue")
#salen graficados los limites de confianza

#si queremos intervalos de prediccion
# a esa varianza, le sumamos sigma cuadrado
LIP=YHAT-2*sqrt(diag(covHat)+sigma(modelo)^2)
LSP=YHAT+2*sqrt(diag(covHat)+sigma(modelo)^2)

plot(x,Y);abline(b, col="red")
points(x,LIP,type="l", col="blue")
points(x,LSP,type="l", col="blue")

#PLOTEA TODO JUNTO
plot(x,Y);abline(b, col="red")
points(x,LI,type="l", col="blue")
points(x,LS,type="l", col="blue")
points(x,LIP,type="l", col="brown")
points(x,LSP,type="l", col="brown")

#no contienen a los puntos, en esa region esta la verdadera media
#la verdadera media esta con un 95% de confianza en ese rango (azul)
#bandas de prediccion son para los datos no para la media, la de confianza es para la media

#confianza asociada a prediccion
confianza=0.99
c=qt(1-(1-confianza)/2,n-p)
LI=YHat-c*sqrt(diag(covYHat))
LS=YHat+c*sqrt(diag(covYHat))

LIp=YHat-c*sqrt(diag(covYHat)+sigma(modelo)^2)
LSp=YHat+c*sqrt(diag(covYHat)+sigma(modelo)^2)

plot(x,Y);abline(b,col="red")
points(x,LI,type="l",col="blue")
points(x,LS,type="l",col="blue")
points(x,LIp,type="l",col="brown")
points(x,LSp,type="l",col="brown")

