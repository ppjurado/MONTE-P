\name{condicion}
\alias{condicion}

\title{
  Generación de condiciones para la simulación.
}
\description{
  Esta función permite establecer condiciones para las variables contínuas de una simulación.
}
\usage{
condicion(variable, condicion, custom = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{
  Variable caracter que indica sobre que variable se quiere aplicar la condición.
}
  \item{condicion}{
  Variable caracter que contenga una condición lógica donde a izquiereda está (sin incluir) la variable contínua, en medio (al principio realmente) el operador lógico que se desee, y a la derecha el valor con el que se debe comparar.
}
  \item{custom}{
  Variable de tipo caracter, reservada para condiciones mas complejas, como por ejemplo involucrar dos variables, establecer un intervalo, o cualquier condición lógica válida que al usuario se le ocurra.
}
}
\details{
  Esta función genera un objeto de tipo \code{condicion}, el cual se puede añadir a un objeto de clase \code{simul} para que a la hora de hacer una simulación con \code{computo}(), los datos generados verifique dicha condición sobre el o los objetos \code{variable} de \code{simul}.
}
\value{
  Un objeto de tipo \code{condicion} que se ha de añadir a un objeto de clase \code{simul}.
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
  Como descrito en "Details", esta función es relevante para las funciones \link{computo}(), \link{variable}(), \link{generador}() y \code{\%_\%}.
}
\examples{

ejemplo<-generador("Ejemplo")

edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
peso<-variable("Peso",dist=list(nombre="Normal",parametros=list(mean=75,sd=7)))
altura<-variable("Altura",dist=list(nombre="Normal",parametros=list(mean=1.75,sd=0.05)))
#Creamos las correlaciones
cors<-matrix(c(1,0.12,0.1,
               0.12,1,0.8,
               0.1,0.8,1),byrow = TRUE,ncol=3)

sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))

condicion1<-condicion("Edad",">40")
condicion2<-condicion(custom="!poblacion[,\"Altura\"]>1.60|!poblacion[,\"Altura\"]<1.85")

ef_edad<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
ef_peso<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
ef_altura<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
asoc<-asociaciones(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)

a<-ejemplo\%_\%edad\%_\%peso\%_\%altura\%_\%cors\%_\%
  sexo\%_\%trabajo\%_\%fuma\%_\%bebe\%_\%
  ef_edad\%_\%ef_peso\%_\%ef_altura\%_\%
  condicion1\%_\%condicion2
computo(10,a)

}
