\name{variable}
\alias{variable}

\title{
Creación de variable
}
\description{
\code{variable} devuelve un objeto de clase \code{variable}, que contiene la información necesaria para su simulación de la variable especificada en una posterior ejecución mediante la función \code{computo}.
}
\usage{
variable(c_variable, dist = list(nombre = "Normal", parametros = list(mean = 0, sd = 1)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c_variable}{
Variable caracter que contiene el nombre asociado al objeto de clase \code{variable} que se está creando.
}
  \item{dist}{
Es un objeto de tipo lista que debe contener los siguientes argumentos que describen unívocamente la distribución que se quiere simular:
    \itemize{
      \item{\code{nombre}}{
    Variable caracter que contiene el nombre en español de una distribución probabilística con la que cuenta la distribución base de \R. La lista con todos los nombres aparece en el apartado "Details".
      }
  
      \item{\code{parametros}}{
    Una variable lista que debe contener los parámetros requeridos por \R para definir la distribución que se quiere simular. En el caso de la normal serán \code{mean} y \code{sd}, así como para una exponencial será \code{rate}.
      }
  }
De forma predeterminada se dan los valores de una distribución \emph{N(0,1)}.
}

}
\details{
El valor de salida de esta función es un objeto por si solo, que no tiene por que usarse en solo una simulación. A la hora de simularlo, debe ir acompañado de un objeto de clase \code{simul}, con el que se vincula mediante el operador especial \code{\%_\%}.

Posee un método \code{print.variable()} (como la mayoría de clases del libro), este método a su vez es usado en \code{print.simul()} a la hora de enseñar toda la información relacionada con la simulación que se está formando. Además de un método \code{plot.variable()}, que permite graficar la función de densidad teórica de la variable descrita.

Una simulación puede tener mas de un objeto \code{variable} (se simulará para cada individuo un valor por cada variable), y para las variables normales se permite establecer una matriz de correlaciones, que también se introducirá en el objeto \code{simul} mediante el operador \code{\%_\%}.

Las distribuciones contempladas, como comentado anteriormente, son todas las que contempla la distribución base de \R, con la siguiente codificación en español:
  \itemize{
    \item{Beta} \code{beta}
    \item{Binomial} \code{binom}
    \item{Cauchy} \code{cauchy}
    \item{Chi2} \code{chisq}
    \item{Exponencial} \code{exp}
    \item{F} \code{f}
    \item{Gamma} \code{gamma}
    \item{Geometrica} \code{geom}
    \item{Hipergeometrica} \code{hyper}
    \item{Lognormal} \code{lnorm}
    \item{Multinomial} \code{multinom}
    \item{Binomial negativa} \code{nbinom}
    \item{Normal} \code{norm}
    \item{Poisson} \code{pois}
    \item{t-student} \code{t}
    \item{Uniforme} \code{unif}
    \item{Weibull} \code{weibull}
  }
}
\value{
Devuelve un objeto de tipo \code{variable}, el cual contiene los atributos necesarios para la simulación de la variable especificada.
}
PONER LAS REFERENCIAS QUE SE VAYAN A USAR EN EL MARCO TEÓRICO
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
COMPROBAR LOS ENLACES A OTROS DOCUMENTOS
\seealso{
  A la hora de simular mediante \link{computo}(), debe ir acompañado de un objeto dado por la función \link{generador}, unidos mediante el operador \link{z\%_\%.Rd}, y tiene los métodos \link{print.variable} \link{plot.variable}. El valor simulado para esta variable puede ser afectado por el uso de la función \link{efectos}() mediante los \link{estratos}.
}

\examples{
ejemplo<-generador("Ejemplo")
edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
computo(100,ejemplo\%_\%edad)
}

\keyword{variable}
\keyword{simulacion}

% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
