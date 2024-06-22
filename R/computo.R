computo<-
  function(diseno,simul){
    
    N<-sum(unlist(diseno))
    n_vars<-length(attr(simul,"variable"))
    n_estr<-length(attr(simul,"estrato"))
    poblacion<-matrix(NA,ncol=n_vars,nrow=N)
    
    dists<-distribuciones(simul)
    ind_norm<-which(dists=="Normal")
    for(i in which(dists!="Normal")){
      rfuncion<-rdistribucion(dists[i])
      params<-c(n=N,attr(attr(simul,"variable")[[i]],"parametros"))
      temp<-do.call(rfuncion,params)
      poblacion[,i]<-temp
    }
    if(length(ind_norm)>0){
      Z<-NULL
      for(i in 1:length(ind_norm)){
        Z<-cbind(Z,rnorm(N))
      }
      mus<-NULL
      sds<-NULL
      for(i in 1:length(ind_norm)){
        mus<-c(mus,attr(attr(simul,"variable")[[i]],"parametros")$mean)
        sds<-c(sds,attr(attr(simul,"variable")[[i]],"parametros")$sd)
      }

      if(is.null(attr(simul,"cors"))){
        
        poblacion[,which(dists=="Normal")]<-t(t(Z*sds))+mus
        
      }else{
        poblacion[,which(dists=="Normal")]<-
          t(t(Z%*%chol(as.matrix(diag(sds)%*%as.matrix(attr(simul,"cors"))%*%diag(sds))))+mus)  
      }
      
    }

    
    if(!is.null(attr(simul,"estrato"))){
      
      proporciones<-attr(simul,"asociaciones")[[1]]
      
      if(n_estr==1){
        temp<-attr(simul,"estrato")[[1]]
        if(is.null(proporciones)){
          proporciones<-attr(temp,"proporciones")
        }
        asoc<-sample(1:length(attr(temp,"modalidades")),N,replace = T,prob = proporciones)
        poblacion<-cbind(poblacion,asoc)
        
      }else{
        if(is.null(proporciones)){
          proporciones<-asociaciones(simul)
        }
        

        if(!is.list(diseno)){
          poblacion<-cbind(poblacion,asignacion(N,proporciones))
        }else{
          if(n_estr==2){
            
            poblacion<-cbind(poblacion,muestro_multinomial_ind_(diseno,proporciones))
          }else{
            poblacion<-cbind(poblacion,muestro_multinomial_ind(diseno,proporciones))
            
          }
        }
      }
            for(i in 1:n_vars){
        temp_efectos<-attr(simul,"efectos")[[i]]
        temp_index<-as.matrix(poblacion[,(n_vars+1):(n_vars+n_estr)])
        if(is.null(dim(temp_index))){poblacion[,i]<-poblacion[,i]+suma_efecto(temp_index,temp_efectos)}
        else{poblacion[,i]<-poblacion[,i]+suma_efecto(temp_index,temp_efectos)}
        
      }
      
    }
    
    
    
    nombres<-NULL
    for(i in 1:n_vars){
      nombres<-c(nombres,attr(simul,"variable")[[i]][1])
    }
    for(i in 1:n_estr){
      nombres<-c(nombres,attr(simul,"estrato")[[i]][1])
    }
    colnames(poblacion)<-nombres
    
    if(!is.null(attr(simul,"condicion"))){
      no_verifican<-NULL
      for(i in 1:length(attr(simul,"condicion"))){
        no_verifican<-c(no_verifican,eval(parse(text=attr(simul,"condicion")[i])))
      }
      no_verifican<-sort(unique(no_verifican))
      if(length(no_verifican>0)){
        poblacion[no_verifican,]<-
          computo(length(no_verifican),simul=simul)
      }
    }
    
    rownames(poblacion)<-NULL
    return(poblacion)
  }

