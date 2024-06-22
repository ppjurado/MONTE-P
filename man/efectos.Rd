\name{efectos}
\alias{efectos}

\title{
  Efectos de los estratos
}
\description{
  \code{efectos}() devuelve un objeto de clase \code{efectos}, el cual es una lista que contiene todos los cruces posibles de todas las cantidades posibles de modalidades de estratos contenidos en el objeto de clase \code{simul} dado como imput. La finalidad es tener una forma accesible (mediante el método \code{"<-"} de \R) de determinar los efectos principales y conjuntos de las modalidades de los estratos contenidos en el objeto de clase \code{simul}
}
\usage{
efectos(simul)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simul}{
  Objeto de clase \code{simul} que contenga al menos un objeto de clase \code{estrato}.
}
}
\details{
  La función \code{efectos}() toma como imput un objeto de tipo \code{simul}, y realiza todas las combinaciones posibles de todos los cruces posibles de modalidades de los objetos de clase \code{estrato} contenidos en el objeto \code{simul}. Como se puede ver en "Value", la lista que da como output tiene tantos elementos como posibles cruces de estratos hay, cada uno de dichos elementos tiene los cruces de 1,2,3... estratos, de modo que el primer elemento contiene un vector por cada estrato (de longitud igual a la cantidad de modalidades de dicho estrato), el segundo elemento contiene tantas matrices como pares de estratos se pueden formar, y así sucesivamente, esto se define mejor en el apartado "Value". 
  
  La finalidad es tener una estructura estandarizada con la que expresar los efectos de los diferentes estratos en las variables contempladas en la simulación, de modo que la salida de esta función es dicha estructura, y modificarla a gusto es tan facil como emplear el método \code{"<-"} de \R con la lista que conforma la estructura.
}

\value{
  El valor de salida es una lista que contiene tantos elementos como posibles cruces de estratos hay, si por ejemplo se tienen los estratos \eqn{\{A,B,C,D,...\}} con modalidades \eqn{\{A_1,A_2,A_3,...,A_I\},\{B_1,B_2,B_3,...,B_J\},\{C_1,C_2,C_3,...,C_K\},\{D_1,D_2,D_3,...,D_L\}}, se obtendría los siguientes elementos:
  
  \itemize{
    \item{Cruce1}{
      \itemize{
        \item{A} Vector con los efectos principales de las modalidades de A
        \item{B} Vector con los efectos principales de las modalidades de B
        \item{C} Vector con los efectos principales de las modalidades de C
        \item{D} Vector con los efectos principales de las modalidades de D
      }
    }
    
    \item{Cruce2}{
      \itemize{
        \item{A x B} Matriz con los efectos conjuntos de las combinaciones de las  modalidades de A y B
        \item{A x C} Matriz con los efectos conjuntos de las combinaciones de las  modalidades de A y C
        \item{...}
      }
    }
    
    \item{Cruce3}{
      \itemize{
        \item{A x B x C} Array de dimensión \eqn{I x J x K} con los efectos conjuntos de las combinaciones de las  modalidades de A, B y C
        \item{A x B x D} Array de dimensión \eqn{I x J x L} con los efectos conjuntos de las combinaciones de las  modalidades de A, B y D
        \item{...}
      }
    }
    
    \item{Cruce4}{
      \itemize{
        \item{A x B x C x D} Array de dimensión \eqn{I x J x K x L} con los efectos conjuntos de las combinaciones de las  modalidades de A, B, C y D
        \item{...}
      }
    }
  }
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
  Pablo Jurado Bascón
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  Esta función requiere de un objeto producido por \link{generador}(), juntado mediante \link{\%_\%} con una serie de objetos producidos por \link{estrato}() para producir la influencia de dichos estratos en los datos simulados de \link{variable}() dentro de las funciones \link{computo}(), \link{suma_efecto}() y \link{efecto_indice}().
}
\examples{
  ejemplo<-generador("Ejemplo")

  edad<-variable("Edad",dist=list(nombre="Normal",parametros=list(mean=42,sd=9)))
  
  sexo<-estrato("Sexo",c("Hombre","Mujer"),c(0.70,0.30))
  trabajo<-estrato("Trabajo",c("Desempleado","Empleado"),c(0.1,0.9))
  fuma<-estrato("Fuma",c("Si","No"),c(0.3,0.7))
  bebe<-estrato("Bebe",c("Mucho","Poco"),c(0.4,0.6))
  
  ef_edad<-efectos(ejemplo\%_\%sexo\%_\%trabajo\%_\%fuma\%_\%bebe)
  
  ef_edad$Cruce2$`Fuma·Bebe`<-matrix(c(15,0,0,0),ncol=2)
  
  computo(1000,ejemplo\%_\%edad\%_\%
               sexo\%_\%trabajo\%_\%fuma\%_\%bebe\%_\%
               ef_edad)
  
}

\keyword{estratos}
\keyword{variable}
\keyword{simulacion}