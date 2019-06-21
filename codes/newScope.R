library(pnn);library(readxl);

# Esta seria la funcion que iria para el paquete
trainNeuralNet <- function(train_set, test_set, category_column = 1, sigma) {
  if(missing(train_set)) 
    stop("The training set shouldn't be empty or null.")
  else if(missing(test_set)) 
    stop("The testing set shouldn't be empty or null.")
  else if(typeof(train_set) != "list") 
    stop("The training set isn't valid.")
  
  pnn = learn(train_set, category.column = category_column)
  
  if(missing(sigma)) 
    pnn = smooth(pnn)
  else 
    pnn = smooth(pnn, sigma)
  
  size <- length(test_set[,1])
  output <- integer(size)
  for (index in 1:size) {
    output[index] <- guess(pnn, test_set[index,])
  }
  
  pnn$output = output
  
  return(pnn)
}

#Data de papa
completeData = data.frame(read_excel("~/Documents/neuronalNetworkRPackageInvestigation/datagnel.xlsx", sheet = "DataJD")) #Carga de la data desde el archivo

testData <- completeData[2:5]
smp_size <- floor(0.75 * nrow(testData))
set.seed(123)
train_ind <- sample(seq_len(nrow(testData)), size = smp_size)
x <- testData[train_ind, ]
y <- data.matrix(testData[-train_ind, 2:4])

test <- trainNeuralNet(x, y, sigma = 0.8)