# Librerias y Opciones-----------------------------------------------------
library(plotly);library(readxl);library(corrplot)
library(psych);library(spdep)
options(digits=5); #Cantidad de decimales a mostrar

# Carga de Datos ----------------------------------------------------------
completeData = data.frame(read_excel("~/Documents/neuronalNetworkRPackageInvestigation/datagnel.xlsx", sheet = "DataJD")) #Carga de la data desde el archivo
pesoFrescoTotal = sum(completeData$PesoFresco) #Calculando el peso total del cultivo

# Estadisticas Decriptivas ------------------------------------------------
estadisticasDescriptivas = describeBy(completeData,completeData$Densidad,digits = 2)

# Datos por densidad ------------------------------------------------------
densidades <- unique(completeData$Densidad)
dataXDensidad <- list()
for (i in densidades){
    dataXDensidad[[i]] <- completeData[completeData$Densidad==i, 2:5]
}

# Correlacion entre variables ---------------------------------------------
listCorrPlots <- list()
par(mfrow = c(1,length(densidades)))
for (i in densidades) {
    correlacion <- cor(dataXDensidad[[i]])
    listCorrPlots[[i]] <- corrplot.mixed(correlacion)
}

# Matriz de Pesos ---------------------------------------------------------
#Calculando distancia maxima por densidad con pitagoras
distanciasXDensidad <- c(30,40,50)
distanciaEntreSurcos <- 100
distMaxEntreVecinosXDens <- rapply(lapply(as.list(distanciasXDensidad),
                                          function(element) sqrt(element^2 + distanciaEntreSurcos^2)),c)

#Matriz de vecinos por densidad
posXYxDensidad <- list()
vecinosXDensidad <- list()
for (i in densidades){
    posXYxDensidad[[i]] <- as.matrix(completeData[completeData$Densidad==i, 6:7])
    vecinosXDensidad[[i]] <- dnearneigh(posXYxDensidad[[i]], 0, distMaxEntreVecinosXDens[i])
}

# Matriz de Distancias-----------------------------------------------------
GetMatrizDistancias <- function(posXY, distanciaMax) {
    parcialDistancias <- round(as.matrix(dist(posXY, method = "euclidean", upper = TRUE, diag=TRUE)),4)
    parcialDistancias[parcialDistancias >= distanciaMax] <- 0
    return(parcialDistancias)
}

matrizDistanciasXDensidad <- list()
for (i in densidades){
    matrizDistanciasXDensidad[[i]] <- GetMatrizDistancias(posXYxDensidad[[i]], distMaxEntreVecinosXDens[i])
}

# Matriz de Pesos ---------------------------------------------------------
matrizParcialPesosXDensidad <- list()
matrizPesosXDensidad <- list()
for (i in densidades){
    matrizParcialPesosXDensidad[[i]] <- as.matrix(1/matrizDistanciasXDensidad[[1]])#solve(matrizDistanciasXDensidad[[i]])
    matrizParcialPesosXDensidad[[i]][matrizParcialPesosXDensidad[[i]] == "Inf"] <- 0
    sumaPesos <- apply(matrizParcialPesosXDensidad[[i]],1,sum)
    matrizPesosXDensidad[[i]] <- matrizParcialPesosXDensidad[[i]]/sumaPesos;
}
