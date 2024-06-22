distribuciones<-
  function(x){
    n_vars<-length(attr(x,"variable"))
    temp<-NULL
    for(i in 1:n_vars){
      temp<-c(temp,attr(attr(x,"variable")[[i]],"distribucion"))
    }
    return(temp)
  }

rdistribucion<-
  function(x){
    paste("r",lista_distribuciones[lista_distribuciones[,1]==x,2],sep="")
  }

"%_%"<-function(e1,e2){
  if(is.matrix(e2)&is.null(attr(e1,"cors"))){
    dimnames(e2)<-list(attr(e1,"variable"),attr(e1,"variable"))
    attr(e1,"cors")<-e2
    return(e1)
  }else{
    ind<-length(attr(e1,class(e2)))+1
    attr(e1,class(e2))[[ind]]<-e2
    return(e1)
  }
}