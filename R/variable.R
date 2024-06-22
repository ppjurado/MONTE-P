variable<-
  function(c_variable,dist=list(nombre="Normal",parametros=list(mean=0,sd=1))){
    temp<-structure(c_variable,class="variable",
                    distribucion=dist$nombre,parametros=dist$parametros)
    return(assign(c_variable,temp))
  }
