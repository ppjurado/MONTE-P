\name{estrato}
\alias{estrato}

\title{
Creaci�n de estrato
}
\description{
\code{estrato()} devuelve un objeto de clase \code{estrato}, que contiene la informaci�n necesaria para su simulaci�n del estrato especificado en una posterior ejecuci�n mediante la funci�n \code{computo}.
}
\usage{
estrato(c_estrato, c_modalidades, proporciones = NULL)
}

\arguments{
  \item{c_estrato}{
  Variable caracter que contiene el nombre asociado al objeto de clase \code{estrato} que se est� creando.
  }
  \item{c_modalidades}{
  Vector de variables caracter que indica las modalidades que puede tomar el estrato que se est�n usando.
  }
  \item{proporciones}{
  Vector num�rico que indica con que probabilidad se presentan las modalidades anteriores en un individuo
  }
}

\details{
  El valor de salida de esta funci�n es un objeto por si solo, que no tiene por que usarse en solo una simulaci�n. A la hora de simularlo, debe ir acompa�ado de un objeto de clase \code{simul} y un objeto de clase \code{variable}, con los que se vincula mediante el operador especial \code{\%_\%}.
  
  Posee un m�todo \code{print.variable()}, este m�todo a su vez es usado en \code{print.simul()} a la hora de ense�ar toda la informaci�n relacionada con la simulaci�n que se est� formando.
  
  Una simulaci�n puede tener mas de un objeto \code{estrato}, se simular� la pertenencia a una modalidad de cada estrato para cada individuo. La pertenencia a una combinaci�n determinada de estratos viene dada por la distribuci�n esperada de cruzar todas las modalidades (con sus probabilidades) de todos los estratos. Si se quiere cambiar dichas probabilidades, (por ejemplo, si se quiere que la pertenencia a la primera modalidad de un estrato \bold{A} influye en las probabilidades de pertenencia de las modalidades de otro estrato \bold{B}), se puede obtener la distribuci�n esperada comentada mediante la funci�n \code{asignacion}() usando el objeto \code{simul} que se est� tratando, ver la ayuda de dicha funci�n para comprender mejor su uso.
  
  Tener tambi�n en cuenta que la pertenencia a un estrato puede afectar al valor de las variables determinadas, esto se hace empleando la funci�n \code{efectos}() con el objeto \code{simul} que se est� tratando, ver la ayuda de dicha funci�n para comprender mejor su uso.
  
}
\value{
Devuelve un objeto de tipo \code{estrato}, el cual contiene los atributos necesarios para la simulaci�n del estrato especificado.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Pablo Pedro Jurado Basc�n
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   A la hora de simular mediante \link{computo}(), debe ir acompa�ado de un objeto dado por la funci�n \link{generador}() y otro dado por la funci�n \link{variable}(), unidos mediante el operador \link{z\%_\%.Rd}, y tiene el m�todo \link{print.variable}. Se puede ahondar mas en el uso de varios objetos \code{estrato} mediante el uso de la funci�n \link{asignacion}() comentado en el apartado de "Details". Para determinar el efecto de dicho estrato, se usa la funci�n \link{efectos}(), comentada en el apartado "Details".
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