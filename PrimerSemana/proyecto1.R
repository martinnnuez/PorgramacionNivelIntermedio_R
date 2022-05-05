#Funcion que haga la prueba t:
myPruebaT <- function(Y1, Y2) {
  n1 = sum(!is.na(Y1)) #is.na me dice si hay algun dato faltante (Y1 = c(2, 3, 3, NA, 4), 
  n2 = sum(!is.na(Y2) )#entonces is.na(Y1) = c(F,F,F,T,F))
  
  m1 = mean(Y1, na.rm = T)
  m2 = mean(Y2, na.rm = T)
  
  S2_1 = var(Y1, na.rm = T)
  S2_2 = var(Y2, na.rm = T) #na.rm me tira error si hay datos faltantes
  
  S2p = (S2_1 * (n1 - 1) + S2_2 * (n2 - 1)) / (n1 + n2 - 2)
  
  myT = abs(m1 - m2) / sqrt(S2p * (1/n1 + 1/n2)) 
  pvalor =(1 - pt(myT, (n1+n2-2))) * 2
  
  return(pvalor)
}

Y1 = c(2, 3, 3, NA, 8, 7, 4)
Y2 = c(3, 4, 4, 9, 8, 7, 5)

myPruebaT(Y1, Y2)

## BAJO HIPOTESIS NULA CON CON VARIANZAS HOMOGENEAS

n1 = 7
n2 = 7

m1 = 30
m2 = 30

Sigma_1 = 1
Sigma_2 = 2

alpha=0.05

#set.seed(56)
#para que los resultados sean reproducibles
misPValores = NULL #algo que tiene algo adentro, ahi voy a meter mis p valores
N = 1000
misPValores=lapply(seq(N),function(x){
  
  X1 = rnorm(n1, m1, Sigma_1)
  X2 = rnorm(n2, m2, Sigma_2)
  
  misPValores =  c(misPValores, myPruebaT(Y1=X1, Y2=X2))
  
})
#for (i in (1:N)) antes de cambiarlo
#mis pvalores no tiene nada y cada vez que pasa por el loop se le va agregando un nuevo 
#pvalor N=10 se le generan 10 valores

length(misPValores)

TET1=sum(misPValores<=alpha)/N
TET1
#suma la cantidad de veces que el pvalor es menor al alfa, es decir rechazo HO
#y como yo se que HO es cierta, es la tasa de error
#cantidad de veces que rechaze HO aunque es cierta


#####
u<-c(1,3,4,5)
a=2
sum(u<=2)
#1

z = numeric(12)
for (i in 1:11) z[i] = z[i] + i

y=1
for (i in 1:10) y = y + i
y

y=0
for (i in 1:length(misPValores)){
  y = y + misPValores[[i]]
return(y)}
y

#environment()


#PARTE DE LA POTENCIA
discrepancias=seq(1:20)
tasasDeRechazo=NULL
tasasDeRechazo=lapply(discrepancias,function(ladiscrepancia)
{
misPValores = NULL 
misPValores=lapply(seq(N),function(x){
  
  X1 = rnorm(n1, m1, Sigma_1)
  X2 = rnorm(n2, m2+ladiscrepancia, Sigma_2)
  
  misPValores =  c(misPValores, myPruebaT(Y1=X1, Y2=X2))
})
TET1=sum(misPValores<=alpha)/N
TET1
})

#cuando termina este ciclo en TET1 estan las tasas de rechazo, y si las dibujo en funcion del vector 
#de discrepancias deberia ver esta curva creciente

plot(discrepancias,tasasDeRechazo,type="l")



######### LO QUE HISO EL PROFE
myPruebaT=function(Y1,Y2) 
{
  n1  = sum(!is.na(Y1))
  n2  = sum(!is.na(Y2))
  S21 = var(Y1, na.rm = T)
  S22 = var(Y2, na.rm = T)
  m1  = mean(Y1,na.rm = T)
  m2  = mean(Y2,na.rm = T)
  S2p =(S21*(n1-1)+S22*(n2-1))/(n1+n2-2)
  myT = abs(m1-m2)/sqrt(S2p*(1/n1+1/n2))
  pvalue=(1-pt(myT,(n1+n2-2)))*2
  return(pvalue)  
}

Y1=c(2,3,3,NA,8,7,4)
Y2=c(3,4,4,9,8,7,5)
myPruebaT(Y1,Y2)

## BAJO HIPOTESIS NULA CON VARIANZAS HOMOGENEAS
n1=20
n2=20
Sig1=1
Sig2=1
m1=30
m2=30
alpha=0.05
N=1000
discrepancias=seq(0,20)
#set.seed(56)
tasasDeRechazo=NULL
tasasDeRechazo=lapply(discrepancias, function(ladiscrepancia)
{
  misPValores=NULL
  misPValores=lapply (seq(N),function(x)
  {
    X1=rnorm(n1,m1,Sig1)
    X2=rnorm(n2,m2+ladiscrepancia,Sig2)
    myPruebaT(X1,X2)
  })
  TETI=sum(misPValores<=alpha)/N
  TETI
})
####################
