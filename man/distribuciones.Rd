\name{distribuciones}
\alias{distribuciones}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Funci�n auxiliar: obtener la distribuci�n de una variable
}
\description{
Funci�n auxiliar de la funci�n \code{computo}(), obtiene las distribuciones de probabilidad de un objeto \code{simul}.
}
\usage{
distribuciones(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Objeto de clase \code{simul} con al menos un objeto de clase \code{variable}.
}
}
\value{
Vector con longitud igual al n�mero de variables que posee el objeto \code{simul}, conteniendo las distribuciones de probabilidad de cada uno.
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
\link{computo}, \code{generador}, \link{variable}, \link{rdistribuci�n}
}
\examples{
ejemplo<-generador("Ejemplo")

edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
peso<-variable("Peso",dist=list(nombre="Normal",parametros=list(mean=75,sd=7)))
altura<-variable("Altura",dist=list(nombre="Normal",parametros=list(mean=1.75,sd=0.05)))

distribuciones(ejemplo\%_\%edad\%_\%peso\%_\%altura)

}
