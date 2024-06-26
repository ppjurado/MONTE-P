\name{muestro_multinomial_ind}
\alias{muestro_multinomial_ind}
\alias{muestro_multinomial_ind_}

\title{
Muestreo Multinomial Independiente
}
\description{
Esta funci�n realiza un Muestreo Multinomial Independiente en un estrato determinado, con un tama�o de muestra especificado para cada modalidad de este, respetando las relaciones entre el resto de estratos de la simulaci�n.

La segunda versi�n de esta funci�n es una adaptaci�n al caso para solo tener 2 estratos.
}
\usage{
muestro_multinomial_ind(diseno, proporciones)
muestro_multinomial_ind_(diseno, proporciones)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{diseno}{
  Objeto de clase \code{list}, contiene tantos elementos como objetos \code{estrato} se quieran tener en cuenta en el muestreo. Todos estos elementos han de ser \code{NULL} a excepci�n del asociado por orden al estrato sobre el que se quiere muestrear. Dicho elemento debe ser entonces un vector con tantos tama�os muestrales como modalidades tenga dicho estrato, distribuyendose de esta forma la muestra. 
}
  \item{proporciones}{
  Objeto objeto de clase \code{asociaciones} (\code{array} o \code{matrix} en el caso de \code{muestro_multinomial_ind_}()) que indique las relaciones muestrales entre las modalidades de los estratos 
}
}
\details{
Esta funci�n es usada en \code{computo}() para el caso que se quiera realizar un muestreo multinomial independiente. Permite, conservando las relaciones muestrales entre las modalidades de los estratos, hacer varias muestras de diferentes tama�os, estando cada una asociada a cada modalidad del estrato sobre el que se desee muestrear.

Para optimizar le c�digo, se tiene dos versiones de la funci�n, una para el caso con dos estratos, y otra para el caso mas general.

Denotar que esta funci�n del libro puede no funcionar como debiera en caso de tener objetos \code{condicion} dentro de la simulaci�n realizada en \code{computo}()
}
\value{
  Devuelve un objeto \code{matrix} que tiene tantas filas como la suma de todos los tama�os muestrales dados, y tantas columnas como estratos se han contemplado. Siendo esta matriz la asignaci�n de cada observaci�n de la simulaci�n a un conjunto de estratos, teniendo en cuenta el dise�o de muestreo.
}

\author{
Pablo Pedro Jurado Basc�n
}

\seealso{
Esta funci�n es usada en \link{computo}(), y a su vez hace uso de la funci�n \code{asignacion}(). Para su uso es necesario el uso de la funci�n (y objetos del mismo nombre) \link{estrato}, y para su segundo argumento es recomendado el uso de la funci�n \link{asociaciones}
}
\examples{

ejemplo<-generador("Ejemplo")

sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))

asoc<-asociaciones(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
muestro_multinomial_ind(list(NULL,NULL,c(5,8),NULL),asoc)

asoc<-asociaciones(ejemplo\%_\%trabajo\%_\%bebe)
muestro_multinomial_ind_(list(NULL,c(5,8)),asoc)
}

\keyword{Simulaci�n}
\keyword{Muestreo}
\keyword{Estrato}