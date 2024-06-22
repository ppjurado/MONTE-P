\name{plot.variable}
\alias{plot.variable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Representación de la densidad teórica de la variable.
}
\description{
Este método \code{plot} toma un objeto de de clase \code{variable} y representa la función de densidad teórica de la distribución y parámetros especificados. 
}
\usage{
plot.variable(x, add = F, cercamiento = 0.99, area_final = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Objeto \code{variable} cuya distribución se quiere graficar.
}
  \item{add}{
Variable lógica, \code{TRUE} si se quiere que superponer la representación a un \code{plot} ya existente.
}
  \item{cercamiento}{
Umbral de area bajo la curva a representar en la selección de la malla. Ver "Details".
}
  \item{area_final}{
Cantidad de area que se quiere en la representación final. Ver "Details".
}
  \item{\dots}{
Parámetros a pasar a la función \code{plot}.
}
}
\details{
Para la representación de la densidad de una función hace falta lo que se conoce como una malla, esta es un vector sobre el que se calcula la función de densidad teórica, lo suficientemente grande (normalmente de tamaño 200) como para que que la representación no sea abrupta.

Para encontrar esta malla, se calcula una media de la distribución provista y a partir de esta, un bucle genera un intervalo alrededor de la amplitud de dos desviaciones típicas, entonces hace un cálculo estimado del área bajo la curva, y si supera el valor \code{cercamiento} se detiene. Con esto se consigue una malla específica de la zona relevante donde representar la variable, pero en el caso de distribuciones asimétricas, esta representación es pobre, es por eso que (en otro bucle) se van eliminando los puntos que estén por debajo de una cierta línea horizontal un poco por encima de la base de la representación. La linea horizontal va subiendo con dicho bucle hasta obtenerse el \code{area_final}
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Pablo Pedro Jurado Bascón
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
