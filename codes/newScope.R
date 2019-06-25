library(pnn);library(readxl);library(dplyr); library(pROC)

#Data de papa
completeData = data.frame(read_excel("~/Documents/neuronalNetworkRPackageInvestigation/datagnel.xlsx", sheet = "DataJD")) #Carga de la data desde el archivo

testData <- completeData[3:5]
testData <- standardize(testData)
testData$Densidad <- completeData[2]$Densidad
testData$Densidad = as.factor(testData$Densidad)
smp_size <- floor(0.75 * nrow(testData))
set.seed(123)
train_ind <- sample(seq_len(nrow(testData)), size = 200)
train_set <- testData[train_ind, ]
test_set <- data.matrix(testData[train_ind, 2:4])

pnn <- trainNeuralNet(train_set, test_set, sigma = -77, category_column = 4)
pnn <- evaluate(pnn)

dataTest <- completeData[2:4]
dir.create("./data")
save(test_set, file = "./data/datatest.rda")
