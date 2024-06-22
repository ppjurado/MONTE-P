\name{print.simul}
\alias{print.simul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.simul(x, efectos = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{efectos}{
%%     ~~Describe \code{efectos} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
function (x, efectos = FALSE) 
{
    cat("La simulación", x[1], "tiene las variables:\n")
    print(attr(x, "variable"))
    if (!is.null(attr(x, "cors"))) {
        cat("con las correlaciones asociadas:\n")
        print(attr(x, "cors"))
        cat("\n")
    }
    cat("Con los estratos:\n")
    print(attr(x, "estrato"))
    cat("Asociados muestralmente por las probabilidades:\n")
    print(attr(x, "asociaciones"))
    if (efectos == TRUE) {
        cat("Con los siguientes efectos asociados:\n")
        for (i in 1:length(attr(x, "variable"))) {
            cat("La variable", attr(x, "variable")[i][[1]], " tiene los efectos: \n")
            print(attr(x, "efectos")[[i]])
        }
    }
    cat("Se le aplican las condiciones:\n")
    print(attr(x, "condiciones"))
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
