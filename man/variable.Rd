\name{variable}
\alias{variable}

\title{
Creaci�n de variable
}
\description{
\code{variable} devuelve un objeto de clase \code{variable}, que contiene la informaci�n necesaria para su simulaci�n de la variable especificada en una posterior ejecuci�n mediante la funci�n \code{computo}.
}
\usage{
variable(c_variable, dist = list(nombre = "Normal", parametros = list(mean = 0, sd = 1)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c_variable}{
Variable caracter que contiene el nombre asociado al objeto de clase \code{variable} que se est� creando.
}
  \item{dist}{
Es un objeto de tipo lista que debe contener los siguientes argumentos que describen un�vocamente la distribuci�n que se quiere simular:
    \itemize{
      \item{\code{nombre}}{
    Variable caracter que contiene el nombre en espa�ol de una distribuci�n probabil�stica con la que cuenta la distribuci�n base de \R. La lista con todos los nombres aparece en el apartado "Details".
      }
  
      \item{\code{parametros}}{
    Una variable lista que debe contener los par�metros requeridos por \R para definir la distribuci�n que se quiere simular. En el caso de la normal ser�n \code{mean} y \code{sd}, as� como para una exponencial ser� \code{rate}.
      }
  }
De forma predeterminada se dan los valores de una distribuci�n \emph{N(0,1)}.
}

}
\details{
El valor de salida de esta funci�n es un objeto por si solo, que no tiene por que usarse en solo una simulaci�n. A la hora de simularlo, debe ir acompa�ado de un objeto de clase \code{simul}, con el que se vincula mediante el operador especial \code{\%_\%}.

Posee un m�todo \code{print.variable()} (como la mayor�a de clases del libro), este m�todo a su vez es usado en \code{print.simul()} a la hora de ense�ar toda la informaci�n relacionada con la simulaci�n que se est� formando. Adem�s de un m�todo \code{plot.variable()}, que permite graficar la funci�n de densidad te�rica de la variable descrita.

Una simulaci�n puede tener mas de un objeto \code{variable} (se simular� para cada individuo un valor por cada variable), y para las variables normales se permite establecer una matriz de correlaciones, que tambi�n se introducir� en el objeto \code{simul} mediante el operador \code{\%_\%}.

Las distribuciones contempladas, como comentado anteriormente, son todas las que contempla la distribuci�n base de \R, con la siguiente codificaci�n en espa�ol:
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
Devuelve un objeto de tipo \code{variable}, el cual contiene los atributos necesarios para la simulaci�n de la variable especificada.
}
PONER LAS REFERENCIAS QUE SE VAYAN A USAR EN EL MARCO TE�RICO
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
COMPROBAR LOS ENLACES A OTROS DOCUMENTOS
\seealso{
  A la hora de simular mediante \link{computo}(), debe ir acompa�ado de un objeto dado por la funci�n \link{generador}, unidos mediante el operador \link{z\%_\%.Rd}, y tiene los m�todos \link{print.variable} \link{plot.variable}. El valor simulado para esta variable puede ser afectado por el uso de la funci�n \link{efectos}() mediante los \link{estratos}.
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
