#' Prediccion de la Red Neuronal
#'
#' @param w Matriz de pesos. (Requerido)
#' @param b Valor del sesgo. (Requerido)
#' @param X Set de datos de entrada (Requerido)
#' @export
# Se encarga de ejecutar la prediccion de la red neuronal probabilística.

trainNeuralNet <- function(train_set, test_set, category_column = 1, sigma) {
  if(missing(train_set))
    stop("The training set shouldn't be empty or null.")
  else if(missing(test_set))
    stop("The testing set shouldn't be empty or null.")
  else if(typeof(train_set) != "list")
    stop("The training set isn't valid.")
  else if (!requireNamespace("pnn", quietly = TRUE))
    stop("Package \"pnn\" needed for this function to work. Please install it.", call. = FALSE)

  print("Begin learning process.")
  pnn = pnn::learn(train_set, category.column = category_column)

  if(missing(sigma)){
    print("Finding optimized minimum value.")
    pnn = pnn::smooth(pnn)
  }
  else
    pnn = pnn::smooth(pnn, sigma)
  pnn = pnn::perf(pnn)
  print("End learning process.")
  print(paste("Success rate: ", pnn$success_rate * 100))
  print("Begin testing set evaluation.")
  size <- length(test_set[,1])
  output <- data.frame(category = numeric(size), propability = numeric(size))
  for (index in 1:size) {
    predict <- pnn::guess(pnn, test_set[index,])
    output$category[index] <- ifelse(!is.na(predict), predict$category, predict)
    output$propability[index] <- ifelse(!is.na(predict), max(predict$probabilities), 0)
  }

  pnn$output = output
  print("End process.")
  return(pnn)
}
