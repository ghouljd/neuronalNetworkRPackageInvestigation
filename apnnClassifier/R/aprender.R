#' Aprendizaje de la red neuronal
#'
#' @param data Set de datos de entrenamiento. (Requerido)
#' @param nombre Nombre de la red entrenada. (No requerido, Por defecto: "Red neuronal probabilística")
#' @param indiceCategoria Indice de la columna que categorizan los datos (No requerido, Por defecto: 1)
#' @return Lista con los datos de la red neuronal probabilística entrenada.
#' @export
# La red neuronal probabilística aprendera como clasificar.

sigmoide <- function(entradas) {
  return(1.0/(1.0+exp(-entradas)))
}

derivadaSigmoide <- function(entradas){
  return(sigmoide(entradas)*(1-sigmoide(entradas)))
}

aprender <- function(red){
  red <- trainedPnn
  if(missing(red)) stop("El parámetro 'red' es requerido.")
  variables <- list()
  variables$numeroCapas <- length(red$numeroDatos)
  variables$learnData <- list()
  i <- 0
  for (entrada in red$setNormalizado) {
    i <- i+1
    variables$sesgos <- rnorm(red$numeroDatos)
    variables$pesos <- rnorm(red$numeroDatos)
    for (item in red$numeroDatos) {
      salida <- pensar(entrada ,variables$pesos)
      error <- entrada - salida
      ajuste <- entrada * (error * derivadaSigmoide(salida))
      variables$pesos <- ajuste
    }
    red$learnData[[i]] <- variables$pesos
  }
}

pensar <- function(entradas, pesos){
  sigmoide(entradas*pesos)
}
