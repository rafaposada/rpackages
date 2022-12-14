% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Varias funciones.R
\name{paste3}
\alias{paste3}
\title{Concatenar dos vectores renglón por renglón.}
\usage{
paste3(..., sep = ", ", reciclar = FALSE)
}
\arguments{
\item{...}{Vectores que se han de concatenar.}
}
\value{
Un vector con los nombres de los valores extremos o una cadena de
}
\description{
Para concatenar dos vectores renglón por renglón sustituyendo los NA por "".
}
\details{
En caso de que los vectores no sean del mismo tamaño, se puede optar por
reciclar los valores hasta completar el vector más largo.
}
\examples{

# Crear datos modelo
x<-1:10
y<-LETTERS[1:10]
y[5]<-NA

# Uso
paste3(x,y)

}
