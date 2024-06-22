\name{distribuciones}
\alias{distribuciones}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Función auxiliar: obtener la distribución de una variable
}
\description{
Función auxiliar de la función \code{computo}(), obtiene las distribuciones de probabilidad de un objeto \code{simul}.
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
Vector con longitud igual al número de variables que posee el objeto \code{simul}, conteniendo las distribuciones de probabilidad de cada uno.
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
\link{computo}, \code{generador}, \link{variable}, \link{rdistribución}
}
\examples{
ejemplo<-generador("Ejemplo")

edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
peso<-variable("Peso",dist=list(nombre="Normal",parametros=list(mean=75,sd=7)))
altura<-variable("Altura",dist=list(nombre="Normal",parametros=list(mean=1.75,sd=0.05)))

distribuciones(ejemplo\%_\%edad\%_\%peso\%_\%altura)

}
