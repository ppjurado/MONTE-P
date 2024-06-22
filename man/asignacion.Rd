\name{asignacion}
\alias{asignacion}

\title{
  Asignación a multiples estratos
}
\description{
La función \code{asignacion}() asigna a un conjunto de \code{N} individuos una combinación de estratos dado una matriz o array de probabilidades. 
}
\usage{
asignacion(N, probs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
  Una variable de clase \code{numeric} que indica el numero de asignaciónes a realizar
}
  \item{probs}{
  Objeto de clase \code{matrix} o \code{array} que contenga las proporciones con las que surgen las modalidades de los estratos
}
}
\details{

  Esta función trata las dimensiones del argumento \code{probs} como estratos, y posibles valores en dicha dimensión como sus modalidades. Cada casilla del argumento indica la proporción (probabilidad en caso de que el argumento en total sume 1) con la que aparece la conjunción de todas las modalidades en las que se situa la casilla.  

  Notar que esta función hace uso en última instancia de la función \code{sample}(), por eso no requiere que el vector de         probabilidades provisto sume 1, la función \code{asignación}()     también se basta con las proporciones con las que aparece cada conjunto de estratos respecto al resto.

}
\value{
  Devuelve una matriz de \code{N} filas y tantas columnas como número de dimensiones tiene \code{probs}. Dicha matriz contiene en cada fila una combinación aleatoria (dependiendo de las probabilidades provistas a la función) de las modalidades de las diferentes dimensiones contenidas en \code{probs}
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
  Esta función puede funcionar tan solo con los argumentos definidos anteriormete, pero dentro de la función \link{computo}(), hace una asignación dependiendo de las probabilidades definidas por los objetos \link{estrato} del objeto \link{simul} dado a \link{computo}(). De forma predeterminada, el argumento \code{probs} viede dado por las probabilidades esperadas según se han dado por los estratos, convertidas en array mediante la función \code{asociacion}()
}
\examples{
temp_prob<-array(1,dim=c(2,5,3))
asignacion(10,temp_prob)
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
