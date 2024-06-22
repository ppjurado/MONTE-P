\name{lista_distribuciones}
\alias{lista_distribuciones}
\docType{data}
\title{
Lista de distribuciones consideradas en español
}
\description{
Esta matriz contiene en cada columna los nombres de todas las distribuciones consideradas por \R, en la primera columna en español y en la segunda en ingles (con la excepción de la distribución hipergeométrica)
}
\usage{data("lista_distribuciones")}
\format{
  The format is:
 chr [1:17, 1:2] "Beta" "Binomial" "Cauchy" "Chi2" "Exponencial" "F" "Gamma" "Geometrica" ...
}
\details{
Esta matriz sirve a la función \code{rdistribucion}() para obtener la función de generación aleatoria de la distribución deseada.

El hecho de que sea una matriz considerada aparte de la función \code{rdistribucion}() es para facilitar su consulta.

La matriz (y el libro) no consideran la distribución hipergeométrica, pues la función \code{rhyper}() usa una nomenclatura diferente para los parámetros.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
