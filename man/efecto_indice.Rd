\name{efecto_indice}
\alias{efecto_indice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selecci�n de efecto dado un �ndice
}
\description{
Dado un �ndice de pertenencia a una serie de modalidades de los estratos a considerar en la simulaci�n, y los efectos de los estratos en dicha simulaci�n, esta funci�n devuelve la suma de todos los efectos asociados a las modalidades presentes en el �ndice y sus combinaciones.
}
\usage{
efecto_indice(index, efectos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index}{
  Vector num�rico que indica la pertenencia a las modalidades de una serie de estratos. 
}
  \item{efectos}{
  Lista o objeto de clase \code{efectos}, con el formato adecuado que considere tantos estratos y modalidades de estos como para concordar con el par�metro \code{index}
}
}
\details{
  Esta funci�n es usada por \code{suma_efecto}() (que a su vez es usada por \code{computo}()) para calcular que efectos (incluidos en el objeto \code{simul}) debe sumar a que combinaciones de modalidades de estratos. Durante la simulaci�n, \code{asignaci�n}() proporciona a cada observaci�n de la simulaci�n los �ndices que indican a que modalidades pertenencen de los objetos \code{estrato}, estos �ndices son luego pasados a la funci�n \code{efecto_indice}() para ver la suma de todos los efectos (del objeto \code{efectos}) se deben sumar dependiendo del �ndice.
}
\value{
  Valor num�rico resultante de la suma de los efectos asociados al �ndice provisto. Vease, si el �ndice es \eqn{i,j,k}, se suma el efecto principal de \eqn{\{i\},\{j\},\{k\}}, el efecto conjunto de \eqn{\{ij\},\{ik\},\{jk\}} y \eqn{\{ijk\}}
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
Como se describe en "Details", esta funci�n interactua con \link{computo},\link{generador},\link{estrato},\link{asignaion} y \link{efectos}
}
\examples{
ejemplo<-generador("Ejemplo")

sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))

efec<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
#Se aplican los cambios pertinentes a la lista efec

indices<-asignacion(10,
ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe\%_\%)

efecto_indice(indices[1,],efec)

}


\keyword{simulacion}
\keyword{efectos}
\keyword{estratos}
