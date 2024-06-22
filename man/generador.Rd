\name{generador}
\alias{generador}

\title{
Función generadora del objeto \code{simul}
}
\description{
El propósito de esta función es crear la base de la simulación, el objeto de clase \code{simul}. 
}
\usage{
generador(nombre)
}

\arguments{
  \item{nombre}{
Variable de tipo caracter que contenga el nombre que se quiere dar a la simulación.
}
}
\details{
Toda simulación/simulaciones que se quiera llevar a cabo con este libro deben empezar con el uso de esta función. Lo único que define de la simulación es su nombre, pero sirve como la base sobre la que se montan el resto de elementos de la simulación mediante el operador \code{\%_\%}. 

Los elementos que se le pueden añadir son los resultados de las funciones \code{variable}(), \code{estrato}(), \code{condiciones}, \code{efectos}() y \code{asignación}, mas una matriz que contenga las correlaciones de los objetos \code{variable} que tengan distribución normal.
}
\value{
  Objeto caracter de clase \code{simul}.
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

\seealso{
Esta función interactua principalmente (como se indica en "Details") con \link{\%_\%}, \link{variable}, \link{estrato}, \link{condicion}, \link{efectos}, \link{asignacion} y \link{computo}.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (nombre) 
{
    current <- nombre
    current <- structure(current, class = "simul")
    invisible(current)
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
