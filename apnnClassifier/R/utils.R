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

inicializar <- function(set) {
  dim = length(set)
  tam = length(set[,1])
  init <- list(
    w=matrix(0, tam, dim),
    b=0
  )
  return(init)
}
