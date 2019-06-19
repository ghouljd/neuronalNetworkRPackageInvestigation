#' Propagacion de la Red Neuronal
#'
#' @param w Matriz de pesos. (Requerido)
#' @param b Valor del sesgo. (Requerido)
#' @param X Set de datos de entrada (Requerido)
#' @param Y Set de datos de salida. (Requerido)
#' @export
# Se encarga de ejecutar la propagacion red neuronal probabilística.

propagar <- function(w, b, X, Y) {
  tam = length(X[,1])
  dw = 0
  # Propagacion hacia adelante
  aprox = sigmoide(w, X, b)
  costo = (-1/tam) * ((Y * log(aprox) + aprox + (1-Y))) * log(1-aprox)

  # Propagacion hacia atras
  for (columna in 1:length(X)) {
    dw[columna] = (1/tam) * (X[columna] * aprox - Y)
  }
  db = (1/tam) * sum(aprox - Y)

  resultado <- list(
    "dw" = dw,
    "db" = db,
    "costo" = costo
  )
  return(resultado)
}

