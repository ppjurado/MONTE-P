print.simul<-
  function(x,efectos=FALSE){
    cat("La simulaci?n",x[1],"tiene las variables:\n")
    print(attr(x,"variable"))
    if(!is.null(attr(x,"cors"))){
      cat("con las correlaciones asociadas:\n")
      print(attr(x,"cors"))
      cat("\n")
    }
    cat("Con los estratos:\n") 
    print(attr(x,"estrato"))
    cat("Asociados muestralmente por las probabilidades:\n")
    print(attr(x,"asociaciones"))
    if(efectos==TRUE){
      cat("Con los siguientes efectos asociados:\n")
      for(i in 1:length(attr(x,"variable"))){
        cat("La variable",attr(x,"variable")[i][[1]]," tiene los efectos: \n")
        print(attr(x,"efectos")[[i]])
      }
    }
    cat("Se le aplican las condiciones:\n")
    print(attr(x,"condiciones"))
  }

print.variable<-
  function(x){
    cat("La variable",x[1],"con distribuci?n",attr(x,"distribucion")[1],"y par?metros:\n")
    print(t(attr(x,"parametros")))
  }


print.estrato<-
  function(x){
    cat("El estrato",x[1],"tiene las modalidades",attr(x,"modalidades"),
        "\n Que est?n presentes en la poblaci?n con las proporciones", 
        attr(x,"proporciones"),"\n")
  }

plot.variable<-
  function(x,add=F,cercamiento=0.99,area_final=0.95,...){ 
    dist<-attr(x,"distribucion")
    
    rfuncion<-paste("r",lista_distribuciones[lista_distribuciones[,1]==dist,2],sep="")
    params<-c(n=120,attr(x,"parametros"))
    muestra<-do.call(rfuncion,params)
    media<-mean(muestra)
    desviaci?n<-sqrt(var(muestra))
    
    dfuncion<-paste("d",lista_distribuciones[lista_distribuciones[,1]==dist,2],sep="")
    params<-attr(x,"parametros")

    i<-1
    area<-0
    while(area<cercamiento){
      malla<-seq(from=media-i*desviaci?n,
                 to=media+i*desviaci?n, length.out = 200)
      amplitud<-abs(malla[1]-malla[2])
      teorica<-do.call(dfuncion,c(list(x=malla),params))
      area<-sum(amplitud*teorica);area
      i<-i+1
    }
    
    puntos<-cbind(malla,teorica)

    horizontal<-puntos[,2][length(puntos[,2])]
    while(area>area_final){
      horizontal<-puntos[,2][length(puntos[,2])-1]
      puntos<-puntos[which(!puntos[,2]<horizontal),]
      area<-sum(puntos[,2]*amplitud)
    }
    if(add==FALSE){
      plot(puntos,type="l",...,main=x,ylab="Densidad",xlab="Valores")
    }else{
      lines(puntos,type="l",...)
    }
  }

