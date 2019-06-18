normalizar <- function(set) {
  return ((set - min(set)) / (max(set) - min(set)))
}

sigmoide <- function(w, X, b) {
  resultado = 1
  for (columna in 1:length(X)) {
    resultado = resultado * 1.0/(1.0+exp(-(w * X[columna] + b)))
  }
  return(resultado)
}

derivada_sigmoide <- function(X, valor) {
  resultado = 1
  for (columna in 1:length(X)) {
    resultado = resultado * X[columna] * valor
  }
  return(resultado)
}

inicializar <- function(dim) {
  init <- list(
    w=integer(dim),
    b=0
  )
  return(init)
}
