#' Funcion Principal de la Red Neuronal
#'
#' @param X_entranamiento Set de datos de entrada de entrenamiento. (Requerido)
#' @param Y_entranamiento Set de datos de salida de entrenamiento. Este campo debe ser un vector de datos binarios. (Requerido)
#' @param X_pruebas Set de datos de entrada de prueba. (Requerido)
#' @param Y_pruebas Set de datos de salida de prueba. Este campo debe ser un vector de datos binarios. (Requerido)
#' @param iteraciones Numero de iteraciones. (No requerido, Por defecto: 2000)
#' @param taza_aprendizaje Taza de aprendizaje (No requerido, Por defecto: 0.005)
#' @export
# Se encarga de correr todas la funciones que permiten clasificacion a traves red neuronal probabilística.

main <- function (X_entranamiento,Y_entranamiento,X_pruebas,Y_pruebas,iteraciones = 2000,taza_aprendizaje = 0.005){
  ini = inicializar(length(X_entranamiento[,1]))

  # Agregar una funcion para validaciones como que el tama.o de los sets sean el mismo,
  # Que realmente sean parametros
  # y tipos de datos recibidos sean correctos

  X_entranamiento = normalizar(X_entranamiento)
  X_pruebas = normalizar(X_pruebas)

  opt = optimizar(ini$w, ini$b, X_entranamiento, Y_entranamiento, iteraciones, taza_aprendizaje)

  Y_prediccion_prueba = predecir(opt$w, opt$b, X_pruebas)
  Y_prediccion_entrenamiento = predecir(opt$w, opt$b, X_entranamiento)

  taza_entrenamiento = 100 - mean(abs(Y_prediccion_entrenamiento - Y_entranamiento)) * 100
  taza_pruebas = 100 - mean(abs(Y_prediccion_prueba - Y_pruebas)) * 100

  print(paste('Taza de entrenamiento: ', taza_entrenamiento))
  print(paste('Taza de pruebas: ', taza_pruebas))
}
