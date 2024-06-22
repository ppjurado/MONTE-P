asociaciones<-function(simul){
  estratos<-attr(simul,"estrato")
  proporciones<-lapply(estratos,function(x){
    return(as.matrix(attr(x,"proporciones")))})
  nombres<-paste(lapply(estratos,function(x) return(x[[1]][1])),
                 lapply(estratos, function(x) return(attr(x,"modalidades"))),sep="=")
  nombres<-do.call(paste,c(as.list(nombres),sep=","))
  nombres<-do.call(paste,c(as.list(c("nombres<-list(",nombres,")")),sep=""))
  eval(parse(text=nombres))
  inicial<-proporciones[[1]]%*%t(proporciones[[2]])
  tabla<-do.call(array,list(data=inicial,
                            dim=unlist(lapply(proporciones,length)),
                            dimnames=nombres),quote = T)
  for(i_estrato in (1:length(estratos))[-(1:2)]){
    temp<-estratos[[i_estrato]]       
    for(i_modalidad in 1:length(attr(temp,"modalidades"))){
      indices <- rep(list(bquote()), length(estratos))
      indices[[i_estrato]] <- i_modalidad

      llamada1<-as.call(c(list(as.name("["),quote(tabla)),indices))
      proporcion<-proporciones[[i_estrato]][i_modalidad]
      llamada2<-as.call(c(list(as.name("*"),llamada1,proporcion)))
      eval(as.call(c(list(as.name("<-"),llamada1,llamada2))))
    }
  }
  return(structure(tabla,class="asociaciones"))
}

asignacion<-function(N,probs){

  casillas<-1:length(probs)
  indices<-expand.grid(lapply(dim(probs),function(x) return(1:x)))
  temp_probs<-apply(indices, 1, function(x){
    eval(as.call(c(list(as.name("["),quote(probs)),x)))
  })
  asignacion1<-sample(casillas,N,replace=T,prob=temp_probs)
  return(indices[asignacion1,])
}

muestro_multinomial_ind<-function(diseno,proporciones){
  tamanos<-diseno[[which(!sapply(diseno, is.null))]]
  indices<-NULL
  for(muestra in 1:length(tamanos)){
    params<-as.vector(rep(T,length(diseno)),mode = "list")
    params[[which(!sapply(diseno, is.null))]]<-muestra
    llamada1<-as.call(c(list(as.name("["),quote(proporciones)),params))
    temp_ind1<-rep(muestra,tamanos[muestra])
    temp_ind2<-eval(as.call(c(list(quote(asignacion)),tamanos[muestra],llamada1)))
    temp_indices<-matrix(NA,nrow =tamanos[muestra],ncol=length(diseno))
    temp_indices[,which(!sapply(diseno, is.null))]<-temp_ind1
    temp_indices[,-which(!sapply(diseno, is.null))]<-as.matrix(temp_ind2)
    
    
    rownames(temp_indices)<-NULL
    indices<-rbind(indices,temp_indices)
  }
  colnames(indices)<-NULL
  return(indices)
}

muestro_multinomial_ind_<-function(diseno,proporciones){
  
  tamanos<-diseno[[which(!sapply(diseno, is.null))]]
  indices<-NULL
  for(muestra in 1:length(tamanos)){
    params<-as.vector(rep(T,length(diseno)),mode = "list")
    params[[which(!sapply(diseno, is.null))]]<-muestra
    llamada1<-as.call(c(list(as.name("["),quote(proporciones)),params))
    
    temp_indices<-matrix(NA,nrow =tamanos[muestra],ncol=length(diseno))
    temp_ind1<-rep(muestra,tamanos[muestra])
    temp_ind2<-eval(as.call(c(list(quote(sample)),x=quote(1:length(eval(llamada1))),
                              tamanos[muestra],replace=T,prob=llamada1)))
    
    temp_indices[,which(!sapply(diseno, is.null))]<-temp_ind1
    temp_indices[,-which(!sapply(diseno, is.null))]<-as.matrix(temp_ind2)
    
    rownames(temp_indices)<-NULL
    
    indices<-rbind(indices,temp_indices)
  }
  colnames(indices)<-NULL
  return(indices)
  
}