\name{\%_\%}
\alias{\%_\%}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Operador Adjuntar (nombre provisional)
}
\description{
Esta función se encarga de ir formando el objeto de clase \code{simul}, permitiendo añadirle todos los componentes relevantes para la simulación, como si de conjugar una frase se tratara.
}
\usage{
e1 \%_\% e2
}

\arguments{
  \item{e1}{
El primer argumento debe ser un objeto de clase \code{simul} donde se pretenda añadir el objeto \code{e2}
}
  \item{e2}{
\code{e2} deberá de ser un objeto de cualquiera de los siguientes clases:
  \itemize{
  \item{\code{variable}} Variable a añadir a la simulación (puede añadirse mas de uno)
  \item{\code{estrato}} Estrato a añadir a la simulación (puede añadirse mas de uno)
  \item{\code{condicion}} Condición que requieran cumplir los datos generados (puede añadirse mas de uno)
  \item{\code{efecto}} Efectos asociados a las diferentes variables (tantos como variables haya)
  \item{\code{asociaciones}} Relaciones muestrales entre las modalidades de los estratos (Solo se puede añadir una)
  \item{\code{matrix}} Matriz simétrica de igual dimensión que el número de variable normales especificadas, esta contiene las correlaciones de dichas variables normales (solo una, y en caso de tener dos o más variables normales)
  }
}
}
\details{
Esta función hace que la formación de las simulaciones (objetos \code{simul}) sea orgánica, facil y entendible. Simplemente hay que poner cada uno de los objetos necesarios uno detrás de otro, con este operador entre cada uno de ellos, para así formar la simulación deseada.

Notar que los efectos deben ser introducidos en el mismo orden que sus variables asociadas, al igual que las correlaciones con las variables normales (aunque no se requiere que las variables normales estén seguidas, ni que estén en una posición concreta; a su vez, la matriz de correlaciones no necesita introducirse en algún momento concreto).
}
\value{
Objeto \code{simul}, originalmente \code{e1}, al que se le ha añadido \code{e2}-
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
Como se ha especificado anteriormente, esta función interactua con: \link{generador}, \link{variable}, \link{estrato}, \link{condicion}, \link{efectos} y  \link{asociaciones}. 
}
\examples{
ejemplo<-generador("Ejemplo")

#Creamos las variables
edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
peso<-variable("Peso",dist=list(nombre="Normal",parametros=list(mean=75,sd=7)))
altura<-variable("Altura",dist=list(nombre="Normal",parametros=list(mean=1.75,sd=0.05)))
#Creamos las correlaciones
cors<-matrix(c(1,0.12,0.1,
               0.12,1,0.8,
               0.1,0.8,1),byrow = TRUE,ncol=3)

#Creamos los estratos
sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))

#Creamos las condiciones

condicion1<-condicion("Edad",">40")
condicion2<-condicion(custom="!poblacion[,\"Altura\"]>1.60|!poblacion[,\"Altura\"]<1.85")

#Creamos los efectos de los estratos, no reales, sino para ver que funciona

ef_edad<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
ef_peso<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
ef_altura<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)  
asoc<-asociaciones(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)


ef_peso$Cruce1$Sexo<-c(7,0)
ef_edad$Cruce2$`Fuma·Bebe`<-matrix(c(30,0,0,0),ncol=2)


a<-ejemplo\%_\%edad\%_\%peso\%_\%altura\%_\%cors\%_\%
  sexo\%_\%trabajo\%_\%fuma\%_\%bebe\%_\%
  ef_edad\%_\%ef_peso\%_\%ef_altura\%_\%asoc
}

\keyword{Simulación}
