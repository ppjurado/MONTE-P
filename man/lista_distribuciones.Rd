\name{lista_distribuciones}
\alias{lista_distribuciones}
\docType{data}
\title{
Lista de distribuciones consideradas en espa�ol
}
\description{
Esta matriz contiene en cada columna los nombres de todas las distribuciones consideradas por \R, en la primera columna en espa�ol y en la segunda en ingles (con la excepci�n de la distribuci�n hipergeom�trica)
}
\usage{data("lista_distribuciones")}
\format{
  The format is:
 chr [1:17, 1:2] "Beta" "Binomial" "Cauchy" "Chi2" "Exponencial" "F" "Gamma" "Geometrica" ...
}
\details{
Esta matriz sirve a la funci�n \code{rdistribucion}() para obtener la funci�n de generaci�n aleatoria de la distribuci�n deseada.

El hecho de que sea una matriz considerada aparte de la funci�n \code{rdistribucion}() es para facilitar su consulta.

La matriz (y el libro) no consideran la distribuci�n hipergeom�trica, pues la funci�n \code{rhyper}() usa una nomenclatura diferente para los par�metros.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}