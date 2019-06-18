#' Prediccion de la Red Neuronal
#'
#' @param w Matriz de pesos. (Requerido)
#' @param b Valor del sesgo. (Requerido)
#' @param X Set de datos de entrada (Requerido)
#' @export
# Se encarga de ejecutar la prediccion de la red neuronal probabilística.

predecir <- function(w,b,X) {
  aprox <- sigmoide(w, X, b)
  return(round(aprox))
}
