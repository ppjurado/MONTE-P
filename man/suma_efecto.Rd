\name{suma_efecto}
\alias{suma_efecto}

\title{
Suma de los efectos dados los índices de la simulación
}
\description{
Esta función toma una matriz que contiene en cada fila el índice que indica la pertenencia de una observación a las diferentes modalidades de un estrato. Devuelve entonces la suma efectos asociada a cada una de las filas. 
}
\usage{
suma_efecto(index, efectos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index}{
Matriz que contiene por filas los ínices de las modalidades de estratos asociadas a un conjunto de observaciones.
}
  \item{efectos}{
Objeto de clase \code{efectos}, el cual concuerde en número de estratos y modalidades con el objeto \code{index}.
}
}
\details{
Aunque el objetivo de esta función es similar a la de \code{indice_efecto}(), su existencia está justificada por que es una versión optimizada de como aplicar \code{indice_efecto} a una gran cantidad de filas.

En vez de aplicar la función a cada fila, lo que hace es registrar todas las combinaciones únicas de modalidades presentes en \code{index}, usa \code{indice_efecto} para cada combinación única. Entonces, usa el método \code{"\["} para aplicar la suma mas facilmente a todas las filas.

Por ejemplo, si se tienen 4 estratos con \eqn{(2,3,3,2)} modalidades cada uno, existen 36 posibles combinaciones de modalidades. Si el objeto \code{index} posee 1000 filas, la función \code{indice_efecto} solo se ejecuta 36 veces, no 1000. De hecho, en caso de que sean menos filas que combinaciones, la función solo ejecuta las combinaciones presentes, luego siempre ejecutará menos veces \code{indice_efecto}() que simplemente aplicandolo por filas. 
}
\value{
Vector numérico que contiene los efectos asociados a las filas provistas en en \code{index}
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

\seealso{
Esta función se ejecuta dentro de \link{computo}, se aplica a las salidas de las funciones \link{efectos} y \link{asignación}.
}
\examples{
ejemplo<-generador("Ejemplo")

sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))

efec<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
#Se aplican los cambios pertinentes a la lista efec

indices<-asignacion(100,
ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe\%_\%)

suma_efecto(indices,efec)
}

\keyword{simulacion}
\keyword{efectos}
\keyword{estrato}