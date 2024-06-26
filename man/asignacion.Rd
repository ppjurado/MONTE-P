\name{asignacion}
\alias{asignacion}

\title{
  Asignaci�n a multiples estratos
}
\description{
La funci�n \code{asignacion}() asigna a un conjunto de \code{N} individuos una combinaci�n de estratos dado una matriz o array de probabilidades. 
}
\usage{
asignacion(N, probs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
  Una variable de clase \code{numeric} que indica el numero de asignaci�nes a realizar
}
  \item{probs}{
  Objeto de clase \code{matrix} o \code{array} que contenga las proporciones con las que surgen las modalidades de los estratos
}
}
\details{

  Esta funci�n trata las dimensiones del argumento \code{probs} como estratos, y posibles valores en dicha dimensi�n como sus modalidades. Cada casilla del argumento indica la proporci�n (probabilidad en caso de que el argumento en total sume 1) con la que aparece la conjunci�n de todas las modalidades en las que se situa la casilla.  

  Notar que esta funci�n hace uso en �ltima instancia de la funci�n \code{sample}(), por eso no requiere que el vector de         probabilidades provisto sume 1, la funci�n \code{asignaci�n}()     tambi�n se basta con las proporciones con las que aparece cada conjunto de estratos respecto al resto.

}
\value{
  Devuelve una matriz de \code{N} filas y tantas columnas como n�mero de dimensiones tiene \code{probs}. Dicha matriz contiene en cada fila una combinaci�n aleatoria (dependiendo de las probabilidades provistas a la funci�n) de las modalidades de las diferentes dimensiones contenidas en \code{probs}
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
  Esta funci�n puede funcionar tan solo con los argumentos definidos anteriormete, pero dentro de la funci�n \link{computo}(), hace una asignaci�n dependiendo de las probabilidades definidas por los objetos \link{estrato} del objeto \link{simul} dado a \link{computo}(). De forma predeterminada, el argumento \code{probs} viede dado por las probabilidades esperadas seg�n se han dado por los estratos, convertidas en array mediante la funci�n \code{asociacion}()
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
