#' Vector a lista de datos.
#' 
#' Convierte los datos de un vector en una lista de datos en español.
#' 
#' La función produce una cadena de texto con cada elemento separado por una 
#' coma y los últimos dos por la palabra "y". Alternativamente se puede 
#' sustituir la palabra y por otra para escribir en otro idioma. El parámetro 
#' 'Y' debe incluir los espacios entre el número y la palabra, también se puede 
#' agregar la coma como parte del separador.
#' 
#' @param x Vector
#' @param Y Caracter que separa los últimos dos datos.
#' 
#' @return Un vector con los valores mayores de otra columna o una cadena de 
#' texto en español.
#' 
#' @examples 
#' 
#' # Crear vector modelo
#' x<-LETTERS[1:10]
#' 
#' # Ejemplo
#' 
#' VectoraLista(x)
#' VectoraLista(x,Y=" and ") # Cambia la lista a inglés

VectoraLista<-function(x,Y=" y "){
  if(!is.vector(x)){stop("x debe ser un vector")}

  Q<-length(x)
  if(Q>2){
    paste0(paste0(x[seq(1,Q-2)],", ",collapse =''),x[Q-1],Y,x[Q])
  } else if(Q==2){paste0(x[1],Y,x[2])} else {paste0(x)}
}