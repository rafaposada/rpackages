#' Valores extremos de una tabla
#' 
#' Extrae los nombres de una tabla de acuerdo a los datos más altos o más bajos.
#' 
#' El argumento 'x' puede tener cualquiera de los siguentes formatos:
#' * Una tabla o un vector con sus datos identificados por nombre. 
#' * Un data.frame del que se puede elegir qué columna se usará para ordenar y 
#' qué columna se usará para extraer la lista de los valores extremos.
#' 
#' Se puede modificar el separador Y=' y ' por Y=' and ' para cambiar el idioma.
#' 
#' @param x Tabla, vector con nombres o data.frame.
#' @param Q Vector que indica qué datos se extraerán de la lista.
#' @param OrdenarPor Número que indica por qué columna se ordenará la tabla.
#' @param ExtraerDe Número que indica de qué columna se extraerá la lista.
#' @param Vector Valor lógico (TRUE o FALSE) que indica si se extraerá la lista 
#' en un vector o como una cadena de texto en español.
#' @param Y Separador de la lista en caso de que sean más de dos elementos y que 
#' el argumento 'Vector' sea FALSE.
#' @param na.rm Valor lógico que indica si se ignora los NA en la columna de 
#' OrdenarPor.
#' 
#' @return Un vector con los nombres de los valores extremos o una cadena de 
#' texto con nombres los mismos valores extremos separados por comas y separando
#' con 'y' los últimos dos términos de forma que se pueda insertar dentro de un
#' documento de Word mediante un archivo rmarkdown en español.

#' @examples 
#' # Ejemplo usando una tabla o un vector con nombres
#' 
#' ## Creación de los datos  
#' x<-c(5,8,9,1,2,3)
#' x<-table(sample.int(6,60,replace = TRUE))
#' 
#' ## Ejemplos
#' names(x)<-LETTERS[1:6]
#' 
#' Mayores(x,1:2) #Arroja las dos letras con más casos
#' Mayores(x,1:3,Vector = TRUE) #Arroja un vector de las tres letras con más casos
#' 
#' # Ejemplo usando un data.frame
#' 
#' x<-data.frame(Variable=LETTERS[1:6],F1=c(78,32,95,22,47,12),F2=c(1,7,2,6,9,3))
#' 
#' ## Ejemplos
#' 
#' mayores(x) # Extrae de la primera columna el valor más grande ordenado por la
#' segunda columna.
#' menores(x,2) #Extrae el segundo valor más pequeño. 
#' mayores(x,1:4,OrdenarPor = 2,ExtraerDe = 3, Y=" and ") #Extrae de la tercer columna los 
#' cuatro valores más grandes ordenados por la segunda columna y en inglés.


mayores<-function(x,Q=1,OrdenarPor=2,ExtraerDe=1,Vector=FALSE,Y=" y ",na.rm = FALSE){
  Mayores=TRUE
  Error=TRUE
  
  #Convierte a data.frame los vectores y las tablas
  if(is.data.frame(x)){
    if(length(names(x))>1){
      Error<-FALSE
      x<-data.frame(x[ExtraerDe],x[OrdenarPor])
      names(x)<-c("Variable","Valores")
    }
  }
  if(is.table(x)|is.vector(x)){
    Error<-FALSE
    x<-data.frame(Variable=names(x),Valores=as.vector(x))
  }
  
  # Manejo de errores    
  
  if(Error==TRUE){
    stop("Los datos deben ser una tabla, un vector con nombres o una data.frame con dos columnas en el que la primera es una lista")
  } else{
    
    if(sum(is.na(x))>0 & na.rm == F){
      stop("No se puede procesar una lista con NA")
    } else {
      if(max(Q)>length(x$Variable)){
        stop("Estas solicitando un número mayor de datos")
      } }
    
  # Ejecución de la función
    x<-suppressWarnings(x[order(x[2],decreasing=Mayores),])
    x<-x[Q,1]
    if(Vector==FALSE){x<-VectoraLista(x,Y)}    }
    return(x)
}

#' @rdname mayores

menores<-function(x,Q=1,OrdenarPor=2,ExtraerDe=1,Vector=FALSE,Y=" y ",na.rm = FALSE){
  Mayores=FALSE
  Error=TRUE
  
  #Convierte a data.frame los vectores y las tablas
  if(is.data.frame(x)){
    if(length(names(x))>1){
      Error<-FALSE
      x<-data.frame(x[ExtraerDe],x[OrdenarPor])
      names(x)<-c("ExtraerDe","OrdenarPor")
    }
  }
  if(is.table(x)|is.vector(x)){
    Error<-FALSE
    x<-data.frame(Variable=names(x),Valores=as.vector(x))
  }
  
  # Manejo de errores    
  
  if(Error==TRUE){
    stop("Los datos deben ser una tabla, un vector con nombres o una data.frame con dos columnas en el que la primera es una lista")
  } else{
    
    if(sum(is.na(x))>0 & na.rm == F){
      stop("No se puede procesar una lista con NA")
    } else {
      if(max(Q)>length(x$ExtraerDe)){
        stop("Estas solicitando un número mayor de datos")
      } }
    
    # Ejecución de la función
    x<-suppressWarnings(x[order(x[2],decreasing=Mayores),])
    x<-x[Q,1]
    if(Vector==FALSE){x<-VectoraLista(x,Y)}    }
  return(x)
}