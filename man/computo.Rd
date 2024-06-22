\name{computo}
\alias{computo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computo de una simulación
}
\description{
Esta función realiza una simulación de las variables, tanto numéricas como estrato, especificadas en un objeto \code{simul}, siguiendo un muestreo especificidado y teniendo en cuenta las relaciones entre las variables categóricas que conforman los estratos.
}
\usage{
computo(diseno, simul)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{diseno}{
Objeto de clase \code{list}, contiene tantos elementos como \code{estrato} contenga el objeto \code{simul}, es decir, contemplados en la simulación. Todos estos elementos han de ser \code{NULL} a excepción del asociado por orden al estrato sobre el que se quiere muestrear. Dicho elemento debe ser entonces un vector con tantos tamaños muestrales como modalidades tenga dicho estrato, distribuyendose de esta forma la muestra. 
}
  \item{simul}{
Objeto de clase \code{simul}, el cual deberá contener los siguientes elementos:

\itemize{
  \item{variable} Al menos una variable que generar
  \item{estrato} cualquier número de estratos
  \item{condicion} tantas como se quieran aplicar
  \item{efecto} tantos como variables haya, y de dimensiones correctas dados los estratos definidos
  \item{asociaciones} opcional, en caso de no darse, se obtienen los predeterminados dada independencia en los estratos
  \item{correlaciones} opcional, solo si se tiene mas de una variable con distribución normal.
}

}
}
\details{
Esta función realiza las siguientes tareas:

\enumerate{
  \item{Genera las variables no normales}
  \item{Genera las variables normales, teniendo en cuenta las correlaciones en caso de haber sido provistas}
  \item{Asocia a cada individuo un conjunto de modalidades de los diferentes estratos acorde tanto al muestreo determinado como al objeto \code{asociaciones} provisto (o calculado por defecto con \code{asociaciones}()). Esto se realiza mediante \code{asignacion}() (en caso de muestreo aleatorio simple) o \code{muestreo_multinomial_ind} en caso de haber determinado un muestreo multinomial independiente}
  \item{En base a las modalidades anteriores, se suman los efectos principales y de interacción entre las variables estrato para todas las observaciones. Esto se realiza mediante la función \code{suma_efecto}}
  \item{Se contrastan las condiciones provistas, y cada individuo que no las cumpla es generado de nuevo hasta cumplirlas. Comentar que este apartado no funciona como debería en caso de usar un muestreo multinomial independiente.}
}
}
\value{
Un objeto \code{matrix} con tantas filas como observaciones determinadas en el diseño muestral, y tantas columnas como \code{variable} y \code{estrato} determinadas en el objeto \code{simul}
}

\author{
Pablo Jurado Bascón
}


\seealso{

Esta función hace uso de: \link{distribuciones}, \link{rdistribucion}, \link{asociaciones}, \link{asignacion}, \link{muestreo_multinomial_ind}, \link{muestreo_multinomial_ind_}, \link{suma_efecto}.

Y requiere como input la intervención de las siguientes funciones: \link{generador}, \link{variable}, \link{estrato}, \link{condicion}, \link{efecto} y  \link{asociaciones}.

}
\examples{
ejemplo<-generador("Ejemplo")

edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
peso<-variable("Peso",dist=list(nombre="Normal",parametros=list(mean=75,sd=7)))
altura<-variable("Altura",dist=list(nombre="Normal",parametros=list(mean=1.75,sd=0.05)))
cors<-matrix(c(1,0.12,0.1,
               0.12,1,0.8,
               0.1,0.8,1),byrow = TRUE,ncol=3)

sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))

ef_edad<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
ef_peso<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
ef_altura<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)  
asoc<-asociaciones(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)


ef_peso$Cruce1$Sexo<-c(7,0)
ef_edad$Cruce2$`Fuma·Bebe`<-matrix(c(30,0,0,0),ncol=2)

a<-ejemplo\%_\%edad\%_\%peso\%_\%altura\%_\%cors\%_\%
  sexo\%_\%trabajo\%_\%fuma\%_\%bebe\%_\%
  ef_edad\%_\%ef_peso\%_\%ef_altura\%_\%asoc

computo(list(NULL,c(5,5),NULL,NULL),a)
}

\keyword{Simulacion}

