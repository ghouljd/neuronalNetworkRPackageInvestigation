#' Entrenamiento de la red neuronal
#'
#' @param data Set de datos de entrenamiento. (Requerido)
#' @param nombre Nombre de la red entrenada. (No requerido, Por defecto: "Red neuronal probabilística")
#' @param indiceCategoria Indice de la columna que categorizan los datos (No requerido, Por defecto: 1)
#' @return Lista con los datos de la red neuronal probabilística entrenada.
#' @export
# Entrenarla red neuronal probabilística.

normalizar <- function(valor) {
  return ((valor - min(valor)) / (max(valor) - min(valor)))
}

entrenar <- function(data,
                     nombre = "Red neuronal probabilística",
                     indiceCategoria = 1){
  if(missing(data)) stop("El parámetro 'data' es requerido.")
  redNeuronal <- list(
    nombre = nombre,
    originalSet = data,
    indiceCategoria = indiceCategoria,
    categorias = unique(data[,indiceCategoria]),
    columnas = length(data[1,]) - 1,
    numeroDatos = length(data[,1])
  )
  redNeuronal$setNormalizado <- redNeuronal$originalSet %>%
                                  (function(x) lapply(x, normalizar))

  return(redNeuronal)
}
