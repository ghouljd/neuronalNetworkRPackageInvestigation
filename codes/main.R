library(pnn);library(readxl);library(dplyr); library(pROC)

# Prueba 1: Data de papa Estandarizada
completeData = data.frame(read_excel("~/Documents/neuronalNetworkRPackageInvestigation/datagnel.xlsx", sheet = "DataJD")) #Carga de la data desde el archivo

tdata <- completeData[3:5]
tdata <- standardize(tdata)
tdata$Densidad <- completeData[2]$Densidad
tdata$Densidad = as.factor(tdata$Densidad)
smp_size <- floor(0.75 * nrow(tdata))
set.seed(123)
train_ind <- sample(seq_len(nrow(tdata)), size = smp_size)
train_set <- tdata[train_ind, ]
test_set <- data.matrix(tdata[-train_ind, 2:3])

pnn <- trainNeuralNet(train_set = train_set, test_set = test_set, category_column = 4)
pnn <- evaluate(pnn)
View(pnn)


# Prueba 2: Data de papa al crudo
completeData = data.frame(read_excel("~/Documents/neuronalNetworkRPackageInvestigation/datagnel.xlsx", sheet = "DataJD")) #Carga de la data desde el archivo

tdata <- completeData[2:5]
tdata$Densidad = as.factor(tdata$Densidad)
smp_size <- floor(0.75 * nrow(tdata))
set.seed(123)
train_ind <- sample(seq_len(nrow(tdata)), size = smp_size)
train_set <- tdata[train_ind, ]
test_set <- data.matrix(tdata[-train_ind, 2:3])
pnn <- trainNeuralNet(train_set = train_set, test_set = test_set, sigma = -77)
pnn <- evaluate(pnn)
View(pnn)


# Prueba 3: Data de prueba del paquetes sin estandarizar
data(trainData, testData)
testData <- as.matrix(testData)
pnn <- trainNeuralNet(train_set = trainData, test_set = testData)
pnn <- evaluate(pnn)
View(pnn)

# Prueba 4: Data de prueba del paquete pnn
data(norms)
smp_size <- floor(0.75 * nrow(norms))
set.seed(123)
train_ind <- sample(seq_len(nrow(norms)), size = smp_size)
train_set <- norms[train_ind, ]
test_set <- data.matrix(norms[-train_ind, 2:3])
pnn <- trainNeuralNet(train_set = train_set, test_set = test_set)
pnn <- evaluate(pnn)
View(pnn)
