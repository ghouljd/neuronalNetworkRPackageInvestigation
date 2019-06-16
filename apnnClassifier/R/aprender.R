#' Aprendizaje de la red neuronal
#'
#' @param data Set de datos de entrenamiento. (Requerido)
#' @param nombre Nombre de la red entrenada. (No requerido, Por defecto: "Red neuronal probabilística")
#' @param indiceCategoria Indice de la columna que categorizan los datos (No requerido, Por defecto: 1)
#' @return Lista con los datos de la red neuronal probabilística entrenada.
#' @export
# La red neuronal probabilística aprendera como clasificar.

sigmoide <- function(z) {
  return(1.0/(1.0+exp(-z)))
}

initializeWithZeros <- function(dim) {
  init <- list(
    w=integer(dim),
    b=0
  )
  return(init)
}

propagate <- function(w, b, X, Y) {
  size = length(X[,1])

  # Forward propagation
  aprox <- sigmoide(w*X$PesoFresco+b)
  cost <- (-1/size) * ((Y * log(aprox) + aprox + (1-Y))) %*% log(1-aprox)

  # Backward propagation
  dw <- (1/size) * (X$PesoFresco * (aprox - Y))
  db <- (1/size) * sum(aprox-Y)

  result <- list(
    dw=dw,
    db=db,
    cost=cost
  )
  return(result)
}

optimize <- function(w, b, X, Y, iterations, learning_rate) {
  cost=integer(iterations)
  for (i in 1:iterations) {
    #Cost and gradient calculation
    prop = propagate(w, b, X, Y)

    w= w - learning_rate * prop$dw
    b= b - learning_rate * prop$db

    cost[i]= prop$cost

    print(paste('costo despues de la iteracion ',i, ': ',cost[i]))
  }

  result <- list(
    w=w,
    b=b,
    dw=prop$dw,
    db=prop$db,
    costos=cost
  )

  return(result)
}

predecir <- function(w,b,X) {
  aprox <- sigmoide(w*X$PesoFresco+b)
  return(round(aprox))
}

execFun <- function (X_train,Y_train,X_test,Y_test,num_iterations = 2000,learning_rate = 0.005){
  init = initializeWithZeros(length(X_train[,1]))

  optimizar = optimize (init$w, init$b, X_train, Y_train, num_iterations, learning_rate)

  print(mean(optimizar$w))
  print(optimizar$b)

  Y_prediction_test = predecir(optimizar$w, optimizar$b, X_test)
  Y_prediction_train = predecir(optimizar$w, optimizar$b, X_train)

  print(Y_prediction_test)

  trainPresition = 100-mean(abs(Y_prediction_train-Y_train))*100;
  testPresition = 100-mean(abs(Y_prediction_test-Y_test))*100;

  print(paste('Precisión de entrenamiento: ', trainPresition))
  print(paste('Precisión de pruebas: ', testPresition))
}

execFun(x_trainSet, y_trainSet, x_testSet, y_testSet, 2000)

