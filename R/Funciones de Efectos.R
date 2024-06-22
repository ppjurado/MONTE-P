efectos<-function(simul){
  estratos<-attr(simul,"estrato")
  modalidades<-lapply(estratos, function(x){return(attr(x,"modalidades"))})
  names(modalidades)<-unlist(estratos)
  combinaciones<-NULL
  for(i in 1:length(estratos)){
    combinaciones<-append(combinaciones,list(combn(1:length(estratos),i)))
    names(combinaciones)[i]<-paste("Cruce",i,sep="")
  } 
  efectos<-NULL
  j<<-1
  lapply(combinaciones, function(x){
    nivel<-NULL
    for(i in 1:dim(x)[2]){
      cruce<-table(expand.grid(modalidades[x[,i]]))-1
      nivel<-append(nivel,list(cruce))
      names(nivel)[i]<-do.call(paste,c(as.list(names(modalidades)[x[,i]]), sep = "?"))
    }
    efectos<<-append(efectos,list(nivel))
    names(efectos)[j]<<-paste("Cruce",j,sep = "")
    j<<-j+1
  })
  class(efectos)<-"efectos"
  return(efectos)
}

efecto_indice<-function(index,efectos){
  suma<-0
  indices<-NULL
  for(i in 1:length(index)){
    indices<-append(indices,list(combn(index,i)))
  }
  
  for(i_cruce in 1:length(index)){
    temp1<-efectos[[i_cruce]]
    for(i_combinaciones in 1:length(temp1)){
      temp2<-temp1[[i_combinaciones]]
      suma<-suma+
        eval(as.call(c(list(as.name("["),quote(temp2)),indices[[i_cruce]][,i_combinaciones])))
    }
  }
  return(suma)
}

suma_efecto<-function(index,efectos){
  temp<-apply(index,2,function(x)sort(unique(x)),simplify = F)
  
  temp<-do.call(expand.grid,temp)
  
  efecXind<-apply(temp, 1, function(x)efecto_indice(x,efectos))

  efecXarray<-array(efecXind,dim=dim(efectos[[length(efectos)]][[1]]))
  
  return(apply(index, 1, function(x){
    eval(as.call(c(list(as.name("["),quote(efecXarray)),x)))
  }))
}