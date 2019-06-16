#' Optimizacion de la Red Neuronal
#'
#' @param w Matriz de pesos. (Requerido)
#' @param b Valor del sesgo. (Requerido)
#' @param X Set de datos de entrada (Requerido)
#' @param Y Set de datos de salida. (Requerido)
#' @param iteraciones Numero de iteraciones. (No requerido, Por defecto: 2000)
#' @param taza_aprendizaje Taza de aprendizaje (No requerido, Por defecto: 0.005)
#' @export
# Se encarga de ejecutar la optimizacion red neuronal probabilística.

optimizar <- function(w, b, X, Y, iteraciones, taza_aprendizaje) {
  costo = integer(iteraciones)
  for (i in 1:iteraciones) {
    prop = propagar(w, b, X, Y)

    w = w - taza_aprendizaje * prop$dw
    b = b - taza_aprendizaje * prop$db

    costo[i] = prop$costo

    print(paste('Costo despues de la iteracion ',i, ': ',costo[i]))
  }

  resultado <- list(
    "w" = w,
    "b" = b,
    "dw" = prop$dw,
    "db" = prop$db,
    "costos" = costo
  )

  return(resultado)
}
