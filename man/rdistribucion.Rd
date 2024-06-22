\name{rdistribucion}
\alias{rdistribucion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Función auxiliar: obtener la función de generación de una variable aleatoria
}
\description{
Función auxiliar de la función \code{computo}(), obtiene las funciones \code{r"distribución"}() dada una variable caracter (obtenida mediante la función \code{distribuciones}()) \code{simul}.}
\usage{
rdistribucion(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Variable caracter que debe contener el nombre de una distribución de probabilidad.
}
}
\details{
El argumento debe ser una variable caracter contemplada en la primera columna de la matriz \code{lista_distribuciones}. 

Sirve para que la función \code{computo}() pueda obtener facilmente la función de \R que genera una determinada distribución.
}
\value{
Función \code{r"distribución"} de \R base.
}
\references{
\link{computo}, \link{variable}, \link{distribuciones}
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    paste("r", lista_distribuciones[lista_distribuciones[, 1] == 
        x, 2], sep = "")
  }
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
