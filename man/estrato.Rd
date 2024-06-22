\name{estrato}
\alias{estrato}

\title{
Creación de estrato
}
\description{
\code{estrato()} devuelve un objeto de clase \code{estrato}, que contiene la información necesaria para su simulación del estrato especificado en una posterior ejecución mediante la función \code{computo}.
}
\usage{
estrato(c_estrato, c_modalidades, proporciones = NULL)
}

\arguments{
  \item{c_estrato}{
  Variable caracter que contiene el nombre asociado al objeto de clase \code{estrato} que se está creando.
  }
  \item{c_modalidades}{
  Vector de variables caracter que indica las modalidades que puede tomar el estrato que se están usando.
  }
  \item{proporciones}{
  Vector numérico que indica con que probabilidad se presentan las modalidades anteriores en un individuo
  }
}

\details{
  El valor de salida de esta función es un objeto por si solo, que no tiene por que usarse en solo una simulación. A la hora de simularlo, debe ir acompañado de un objeto de clase \code{simul} y un objeto de clase \code{variable}, con los que se vincula mediante el operador especial \code{\%_\%}.
  
  Posee un método \code{print.variable()}, este método a su vez es usado en \code{print.simul()} a la hora de enseñar toda la información relacionada con la simulación que se está formando.
  
  Una simulación puede tener mas de un objeto \code{estrato}, se simulará la pertenencia a una modalidad de cada estrato para cada individuo. La pertenencia a una combinación determinada de estratos viene dada por la distribución esperada de cruzar todas las modalidades (con sus probabilidades) de todos los estratos. Si se quiere cambiar dichas probabilidades, (por ejemplo, si se quiere que la pertenencia a la primera modalidad de un estrato \bold{A} influye en las probabilidades de pertenencia de las modalidades de otro estrato \bold{B}), se puede obtener la distribución esperada comentada mediante la función \code{asignacion}() usando el objeto \code{simul} que se esté tratando, ver la ayuda de dicha función para comprender mejor su uso.
  
  Tener también en cuenta que la pertenencia a un estrato puede afectar al valor de las variables determinadas, esto se hace empleando la función \code{efectos}() con el objeto \code{simul} que se esté tratando, ver la ayuda de dicha función para comprender mejor su uso.
  
}
\value{
Devuelve un objeto de tipo \code{estrato}, el cual contiene los atributos necesarios para la simulación del estrato especificado.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Pablo Pedro Jurado Bascón
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   A la hora de simular mediante \link{computo}(), debe ir acompañado de un objeto dado por la función \link{generador}() y otro dado por la función \link{variable}(), unidos mediante el operador \link{z\%_\%.Rd}, y tiene el método \link{print.variable}. Se puede ahondar mas en el uso de varios objetos \code{estrato} mediante el uso de la función \link{asignacion}() comentado en el apartado de "Details". Para determinar el efecto de dicho estrato, se usa la función \link{efectos}(), comentada en el apartado "Details".
}
\examples{
  ejemplo<-generador("Ejemplo")
  edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
  sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.50,0.50))
  fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
  ef_edad<-efectos(ejemplo\%_\%sexo\%_\%fuma)
  computo(100,ejemplo\%_\%edad\%_\%sexo\%_\%fuma\%_\%ef_edad)
}

\keyword{estrato}
\keyword{simulacion}