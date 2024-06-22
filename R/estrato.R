estrato<-
  function(c_estrato,c_modalidades,proporciones=NULL){
    temp<-structure(c_estrato,class="estrato",
                    modalidades=c_modalidades,
                    proporciones=proporciones)
    return(assign(c_estrato,temp))
  }