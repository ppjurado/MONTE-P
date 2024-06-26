\name{plot.variable}
\alias{plot.variable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Representaci�n de la densidad te�rica de la variable.
}
\description{
Este m�todo \code{plot} toma un objeto de de clase \code{variable} y representa la funci�n de densidad te�rica de la distribuci�n y par�metros especificados. 
}
\usage{
plot.variable(x, add = F, cercamiento = 0.99, area_final = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Objeto \code{variable} cuya distribuci�n se quiere graficar.
}
  \item{add}{
Variable l�gica, \code{TRUE} si se quiere que superponer la representaci�n a un \code{plot} ya existente.
}
  \item{cercamiento}{
Umbral de area bajo la curva a representar en la selecci�n de la malla. Ver "Details".
}
  \item{area_final}{
Cantidad de area que se quiere en la representaci�n final. Ver "Details".
}
  \item{\dots}{
Par�metros a pasar a la funci�n \code{plot}.
}
}
\details{
Para la representaci�n de la densidad de una funci�n hace falta lo que se conoce como una malla, esta es un vector sobre el que se calcula la funci�n de densidad te�rica, lo suficientemente grande (normalmente de tama�o 200) como para que que la representaci�n no sea abrupta.

Para encontrar esta malla, se calcula una media de la distribuci�n provista y a partir de esta, un bucle genera un intervalo alrededor de la amplitud de dos desviaciones t�picas, entonces hace un c�lculo estimado del �rea bajo la curva, y si supera el valor \code{cercamiento} se detiene. Con esto se consigue una malla espec�fica de la zona relevante donde representar la variable, pero en el caso de distribuciones asim�tricas, esta representaci�n es pobre, es por eso que (en otro bucle) se van eliminando los puntos que est�n por debajo de una cierta l�nea horizontal un poco por encima de la base de la representaci�n. La linea horizontal va subiendo con dicho bucle hasta obtenerse el \code{area_final}
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Pablo Pedro Jurado Basc�n
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{variable}
}
\examples{
edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
plot(edad)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
