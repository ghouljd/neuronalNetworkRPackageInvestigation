#' Prediccion de la Red Neuronal
#'
#' @param w Matriz de pesos. (Requerido)
#' @param b Valor del sesgo. (Requerido)
#' @param X Set de datos de entrada (Requerido)
#' @export
# Se encarga de ejecutar la prediccion de la red neuronal probabilística.

evaluate <- function(pnn) {
  if(missing(pnn))
    stop("The pnn parameter is required.")
  else if(is.null(pnn$output))
    stop("The pnn parameter should be a neural net trained.")
  else if (!requireNamespace("pROC", quietly = TRUE))
    stop("Package \"pROC\" needed for this function to work. Please install it.", call. = FALSE)
  else if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package \"dplyr\" needed for this function to work. Please install it.", call. = FALSE)

  plot(dplyr::select(pnn$set, -pnn$category.column))

  roc.multi <- pROC::multiclass.roc(pnn$output$category, pnn$output$propability, percent=TRUE)
  rs <-roc.multi$rocs
  for (index in 1:length(rs)) {
    pROC::plot.roc(rs[[index]])
  }
  sapply(1:length(rs),function(i) pROC::lines.roc(rs[[i]],col=i))

  pnn$evaluation = roc.multi

  return(pnn)
}
