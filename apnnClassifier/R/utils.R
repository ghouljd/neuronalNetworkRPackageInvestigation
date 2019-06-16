normalizar <- function(set) {
  return ((set - min(set)) / (max(set) - min(set)))
}

sigmoide <- function(z) {
  return(1.0/(1.0+exp(-z)))
}

inicializar <- function(dim) {
  init <- list(
    w=integer(dim),
    b=0
  )
  return(init)
}
