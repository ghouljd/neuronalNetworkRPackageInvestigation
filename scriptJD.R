# Librerias y Opciones-----------------------------------------------------
library(plotly);library(readxl);library(corrplot)
library(psych);library(spdep)
options(digits=5); #Cantidad de decimales a mostrar

# Carga de Datos ----------------------------------------------------------
completeData = data.frame(read_excel("~/Documents/Tesis/datagnel.xlsx", sheet = "DataJD")) #Carga de la data desde el archivo
pesoFrescoTotal = sum(completeData$PesoFresco) #Calculando el peso total del cultivo

# Estadisticas Decriptivas ------------------------------------------------
estadisticasDescriptivas = describeBy(completeData,completeData$Densidad,digits = 2)

# Datos por densidad ------------------------------------------------------
densidades <- unique(completeData$Densidad)
dataXDensidad <- list()
for (i in densidades){
    dataXDensidad[[i]] <- completeData[completeData$Densidad==i, 3:5]
}

# Correlacion entre variables ---------------------------------------------
listCorrPlots <- list()
par(mfrow = c(1,length(densidades)))
for (i in densidades) {
    correlacion <- cor(dataXDensidad[[i]])
    listCorrPlots[[i]] <- corrplot.mixed(correlacion)
}

# Histogramas -------------------------------------------------------------
histPFxDensidad <- plot_ly(alpha = 0.6) %>%
                           add_histogram(x = ~dataXDensidad[[1]]$PesoFresco, name = "Peso Fresco - Densidad 1") %>%
                           add_histogram(x = ~dataXDensidad[[2]]$PesoFresco, name = "Peso Fresco - Densidad 2") %>%
                           add_histogram(x = ~dataXDensidad[[3]]$PesoFresco, name = "Peso Fresco - Densidad 3") %>%
                           layout(barmode = "overlay")

histDMPxDensidad <- plot_ly(alpha = 0.6) %>%
                            add_histogram(x = ~dataXDensidad[[1]]$DMP, name = "Diametro Medio Ponderado - Densidad 1") %>%
                            add_histogram(x = ~dataXDensidad[[2]]$DMP, name = "Diametro Medio Ponderado - Densidad 2") %>%
                            add_histogram(x = ~dataXDensidad[[3]]$DMP, name = "Diametro Medio Ponderado - Densidad 3") %>%
                            layout(barmode = "overlay")

histCPxDensidad <- plot_ly(alpha = 0.6) %>%
                           add_histogram(x = ~dataXDensidad[[1]]$CantPapas, name = "Cantidad de Papas - Densidad 1") %>%
                           add_histogram(x = ~dataXDensidad[[2]]$CantPapas, name = "Cantidad de Papas - Densidad 2") %>%
                           add_histogram(x = ~dataXDensidad[[3]]$CantPapas, name = "Cantidad de Papas - Densidad 3") %>%
                           layout(barmode = "overlay")

subplot(histPFxDensidad, histDMPxDensidad, histCPxDensidad)

# Diagramas de Cajas y Bigotes --------------------------------------------
boxPFxDensidad <- plot_ly(y = ~dataXDensidad[[1]]$PesoFresco, type = "box", name = "Peso Fresco - Densidad 1") %>%
                  add_trace(y = ~dataXDensidad[[2]]$PesoFresco, name = "Peso Fresco - Densidad 2") %>%
                  add_trace(y = ~dataXDensidad[[3]]$PesoFresco, name = "Peso Fresco - Densidad 3")

boxDMPxDensidad <- plot_ly(y = ~dataXDensidad[[1]]$DMP, type = "box", name = "Diametro Medio Ponderado - Densidad 1") %>%
                   add_trace(y = ~dataXDensidad[[2]]$DMP, name = "Diametro Medio Ponderado - Densidad 2") %>%
                   add_trace(y = ~dataXDensidad[[3]]$DMP, name = "Diametro Medio Ponderado - Densidad 3")

boxCPxDensidad <- plot_ly(y = ~dataXDensidad[[1]]$CantPapas, type = "box", name = "Cantidad de Papas - Densidad 1") %>%
                  add_trace(y = ~dataXDensidad[[2]]$CantPapas, name = "Cantidad de Papas - Densidad 2") %>%
                  add_trace(y = ~dataXDensidad[[3]]$CantPapas, name = "Cantidad de Papas - Densidad 3")

subplot(boxPFxDensidad, boxDMPxDensidad, boxCPxDensidad)

# Mapeo de plantas --------------------------------------------------------
#Por DMP
listBubblePlots <- list()
for (i in densidades){
    listBubblePlots[[i]] <- plot_ly(completeData[completeData$Densidad==i,], 
                                    x = ~PosX, y = ~PosY, text = ~Planta, 
                                    type = 'scatter', mode = 'markers',
                                    marker = list(size = ~DMP, opacity = 0.5),
                                    name = paste("Densidad", i)) %>%
                            layout(title = 'Diametro Ponderado Medio por Planta',
                                   xaxis = list(showgrid = FALSE),
                                   yaxis = list(showgrid = FALSE))
}
subplot(listBubblePlots[[1]],listBubblePlots[[2]],listBubblePlots[[3]])

#Por Cantidad de Papas
listBubblePlots <- list()
for (i in densidades){
    listBubblePlots[[i]] <- plot_ly(completeData[completeData$Densidad==i,], 
                                    x = ~PosX, y = ~PosY, text = ~Planta, 
                                    type = 'scatter', mode = 'markers',
                                    marker = list(size = ~CantPapas, opacity = 0.5),
                                    name = paste("Densidad", i)) %>%
                            layout(title = 'Cantidad de Papas por Planta',
                                   xaxis = list(showgrid = FALSE),
                                   yaxis = list(showgrid = FALSE))
}
subplot(listBubblePlots[[1]],listBubblePlots[[2]],listBubblePlots[[3]])

#Por Peso Fresco
listBubblePlots <- list();
for (i in densidades){
    listBubblePlots[[i]] <- plot_ly(completeData[completeData$Densidad==i,], 
                                    x = ~PosX, y = ~PosY, text = ~Planta, 
                                    type = 'scatter', mode = 'markers',
                                    marker = list(size = ~PesoFresco, opacity = 0.5),
                                    name = paste("Densidad", i)) %>%
                            layout(title = 'Peso Fresco por Planta',
                                       xaxis = list(showgrid = FALSE),
                                       yaxis = list(showgrid = FALSE))
}
subplot(listBubblePlots[[1]],listBubblePlots[[2]],listBubblePlots[[3]])

# Diagrama de Barras ------------------------------------------------------
totalCantPapasXDensidad <- rapply(lapply(dataXDensidad, function(element) sum(element$CantPapas)),c)
totalDMPXDensidad <- rapply(lapply(dataXDensidad, function(element) mean(element$DMP)),c)
totalPesoFrescoXDensidad <- rapply(lapply(dataXDensidad, function(element) sum(element$DMP)),c)
nameDensidades <- rapply(as.list(densidades), function(i) paste("Densidad",i))

barCantPapasXDensidad <- plot_ly(
    x = ~nameDensidades,
    y = ~totalCantPapasXDensidad,
    name = "Numero Total de Tuberculos por Densidad",
    type = "bar"
)
barDMPXDensidad <- plot_ly(
    x = ~nameDensidades,
    y = ~totalDMPXDensidad,
    name = "Diametro Ponderado Medio por Densidad",
    type = "bar"
)
barPesoFrescoXDensidad <- plot_ly(
    x = ~nameDensidades,
    y = ~totalPesoFrescoXDensidad,
    name = "Total de Peso Fresco por Densidad",
    type = "bar"
)
subplot(barCantPapasXDensidad, barDMPXDensidad, barPesoFrescoXDensidad)

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

# Indices de Moran --------------------------------------------------------


