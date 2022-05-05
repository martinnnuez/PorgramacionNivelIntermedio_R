#22-10-20

resultados=sapply(seq(1:1000),function(i){
tryCatch(
  {
    result=NA
    #CODIGO
    if(i==800) stop(paste("Ocurrio un error en el paso",i))
    result=i
    
  }, error=function(e){print(e)}
)
  result
})

#cuando yo lo corre en 800 se genera un error pero deberia seguir corriendo.
resultados

#tenemos resultados excepto en ese caso que ocurrio un error
#y sabemos donde ocurre el error.
#modelo no converge o falta variable un una funcion 
#aveces tenemos condicion que no ocurre casi nunca y cuando ocurre

#esta estructura hay que manejarla, funamental  

######### la que hiso el profe
resultados=sapply(seq(1000),function(i){
  tryCatch(
    {
      result=NA
      #CODIGO
      if(i==800) stop(paste("Ocurrio un error en el paso",i))
      result=i
      
    },error=function(e){print(e)}
  )
  result
})

#Datos faltantes
which(is.na(resultados))#dice donde esta el faltante

resultados[complete.cases(resultados)]
#devuelve vector sin datos faltantes

#si es matriz
datos=datos[complete.cases(datos[,c(1,2,3,4,5)],)]
#digo que quiero la 1,2,3,4,5....

