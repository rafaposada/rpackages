#' Sustituir valores en un DF
#' 
#' Sustituye un texto en cualquier parte de un data.frame por otro texto.
#' 
#' OJO: Convierte todas las columnas a caracteres.
#' 
#' @param Previo Caracter que se va a cambiar. 
#' @param Nuevo Caracter que se va a incluir
#' @param Donde Data.frame o vector en el que se hará la sustitución
#' 
#' @return Un vector o un data.frame
#' 
#' @examples
#' 
#' # Crear datos modelo
#' x<-data.frame(Variable=LETTERS[1:6],F1=factor(c(2,3,5,32,6,2)),F2=c(1,7,2,6,9,3))
#' 
#' # Ejemplo 
#' sustituye(2,"R",x)

sustituye<-function(Previo,Nuevo,Donde){
  data.frame(lapply(Donde, function(x) {
    gsub(Previo,Nuevo,x)              }))
}


#' Convertir a numérico para data.frames.
#' 
#' Convierte un data.frame o un vector a valores numéricos.
#' 
#' Esta función es una cápsula para la función as.numeric que controla a qué 
#' columnas se aplica la conversión, introduciendo NA en los datos que no se 
#' pueden convertir. Cuando se le aplica a los factores, primero los convierte a
#' caracter y luego a numérico para respetar el valor observado. 
#' 
#' La diferencia entre las dos funciones es que _numerico_ convierte las 
#' columnas especificadas del dataframe y lo arroja completo, mientras que 
#' _numerico2_ sólo arroja las columnas convertidas. Cuando se usa con vectores 
#' el resultado difiere de as.numeric en que convierte los factores en el valor 
#' observado y no en el número subyacente y en caso de que sean letras los 
#' convierte en NA.
#' 
#' @param x Vector o data.frame a procesar 
#' @param Columnas Columnas que se han de procesar. Aplica sólo si es data.frame.
#' 
#' @return Un data.frame o un vector
#' 
#' @examples 
#' 
#' Crear data.frame modelo
#' W<-data.frame( Variable=LETTERS[1:6],
#'                F1=factor(c(2,3,5,3,2,2)),
#'                F2=c(1,7,2,6,9,3),
#'                F3=c("Juan","Pedro","María","Fernanda","José","Pía"),
#'                F4=factor(c("H","H","M","M","H","M")))
#' 
#' #Uso:
#' 
#' #Genera el mismo data.frame con datos numéricos. Si hay columnas 
#' de caracteres introduce NA:
#' numerico(W) 
#' 
#' # Ambos ejemplos hacen lo mismo: Convierten las columnas 2:3 y las incorporan
#' al data.frame original 
#' W[2:3]<-numerico2(W,2:3)
#' W<-numerico(W,2:3)
#' 
#' # Si es un vector, genera un vector:
#' numerico(W$F1)

numerico<-function(x,Columnas){
  if(is.data.frame(x)){
    z<-data.frame(lapply(x[Columnas],as.character))
    z<-data.frame(lapply(z,as.numeric))
    x[Columnas]<-z
    return(x)
  } else if(is.vector(as.character(x))){
    unlist(lapply(as.character(x),as.numeric))
  } else {
    stop("Es necesario x sea un vector o un data.frame")
  }}

#' @rdname numerico

numerico2<-function(x,Columnas){
  if(is.data.frame(x)){
    x<-data.frame(lapply(x[Columnas],as.character))
    x<-data.frame(lapply(x[Columnas],as.numeric))
    return(x)
  } else if(is.vector(as.character(x))){
    unlist(lapply(as.character(x),as.numeric))
  } else {
    stop("Es necesario x sea un vector o un data.frame")
  }}


#' Concatenar dos vectores renglón por renglón.
#' 
#' Para concatenar dos vectores renglón por renglón sustituyendo los NA por "".
#' 
#' En caso de que los vectores no sean del mismo tamaño, se puede optar por 
#' reciclar los valores hasta completar el vector más largo.
#' 
#' @param ... Vectores que se han de concatenar.
#' 
#' @return Vector con cadenas de texto. 
#' 
#' @examples
#' 
#' # Crear datos modelo
#' x<-1:10
#' y<-LETTERS[1:10]
#' y[5]<-NA
#' 
#' # Uso
#' paste3(x,y)
#' 

paste3 <- function(...,sep=", ",reciclar=FALSE) {
  L <- list(...)
  if(length(unique(lengths(L))) == 1 | reciclar ==TRUE){
     L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
     ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                 gsub(paste0(sep,sep),sep,
                      do.call(paste,c(L,list(sep=sep)))))
     is.na(ret) <- ret==""
     ret
  } else {
    mensaje<-paste("Los vectores tiene dimensiones diferentes:",VectoraLista(lengths(list(...))))
    stop(mensaje)}}

#' Vector de modas
#' 
#' Función genérica para obtener los valores que más se repiten.
#' 
#' @param x Un vector con datos de cualquier tipo. 
#' 
#' @return Un vector con los valores que más se repiten.
#' 
#' @examples 
#' 
#' x <- c(rep(1,3),rep(2,4),rep(3,2),rep(4,1),rep(5,4),6)
#' 
#' Modas(x)

Modas <- function(x) { 
  ux <- unique(x) 
  tab <- tabulate(match(x, ux)) 
  ux[tab == max(tab)] }


#' Permutación
#' 
#' Función genérica para realizar una permutación de 'x' tomando 'y' (recuerden 
#' que en la permutación el órden sí importa)
#' 
#' @param x Cantidad de elementos en el universo 
#' @param y Cantidad de elementos a ordenar (sin remplazo)  
#' 
#' @return Un valor numérico del número de combinaciones 
#' 
#' @examples 
#' 
#' permutación(5,3)

permutacion<-function(x,y){choose(x,y)*factorial(y)}

#' Título
#' 
#' Descripción breve.
#' 
#' Detalles
#' 
#' @param na.rm Valor lógico que indica si se ignora los NA en la columna de 
#' 
#' @return Un vector con los nombres de los valores extremos o una cadena de 
#' 
#' @examples 

z.test<-function(x,p,na = TRUE) {
  m<-mean(x,na.rm = na)
  s<-sd(x,na.rm = na)/sqrt(length(x[!is.na(x)]))
  z<- qnorm((1+p)/2)
  r<- paste(round(m-s*z,6)," < \U00B5 < ",round(m+s*z,6))
  return(r)
}


