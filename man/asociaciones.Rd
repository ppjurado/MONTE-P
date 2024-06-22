\name{asociaciones}
\alias{asociaciones}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Asociaciones esperadas entre estratos
}
\description{
  Esta función genera la distribución de probabilidades esperada de cruzar las modalidades de los diferentes estratos. Es decir, genera una tabla de contingencia que contiene las asociaciones entre las modalidades de los estratos como si existiera independencia entre dichos estratos.
}
\usage{
asociaciones(simul)
}

\arguments{
  \item{simul}{
  Objeto de clase \code{simul} que contenga al menos dos objetos de clase \code{estrato}.
}
}
\details{
  Esta función da como resultado la tabla de contingencia multidimensional que da las relaciones esperadas bajo independencia entre las modalidades de las diferentes variables cualitativas (en este caso, estratos) que contiene la simulación. Es decir, la tabla asigna una modalidades u otras dependiendo solamente de la distribución marginal de estas (la determinada por el objeto \code{estrato}), y no del resto de estratos que tenga asociados.
  
  Se puede usar también para obtener la matriz o el array de las dimensiones adecuadas, para proporcionar un objeto de clase \code{asociaciones} al objeto \code{simul}, con el cual definir las relaciones de dependencia entre las modalidades de los estratos.
}
\value{
  Matriz o Array de clase \code{asociaciones} que contiene las relaciones esperadas entre las modalidades de los estratos del objeto \code{simul}, dadas sus distribuciones marginales.
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
Esta función requiere de un objeto \code{simul} generado por la función \link{generador}, junto con al menos dos objetos de clase \link{estrato}, asociados mediante la función \link{\%_\%}. Es usada en la función \link{computo}() en caso de que el objeto \code{simul} proporcionado no tenga un objeto de clase \code{asociaciones}.
}
\examples{
ejemplo<-generador("Ejemplo")

sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))

asociaciones(a)

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
