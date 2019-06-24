#' Prediccion de la Red Neuronal
#'
#' @param w Matriz de pesos. (Requerido)
#' @param b Valor del sesgo. (Requerido)
#' @param X Set de datos de entrada (Requerido)
#' @export
# Se encarga de ejecutar la prediccion de la red neuronal probabilística.

standardize <- function(set, type = "punctual") {
  if(missing(set))
    stop("The set parameter shouldn't be empty or null.")
  else if(type != "punctual" && type != "scale")
    stop("Type parameter isn't valid.")

  if (type == "punctual"){
    for (index in 1:length(set)) {
      set[[index]] = (set[[index]] - mean(set[[index]])) / sd(set[[index]])
    }
    return(set)
  }
  else return ((set - min(set)) / (max(set) - min(set)))
}
