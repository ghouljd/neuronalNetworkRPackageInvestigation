createSetData <- function(completeData){
  smp_size <- floor(0.75 * nrow(completeData))

  set.seed(123)
  train_ind <- sample(seq_len(nrow(completeData)), size = smp_size)

  train <- completeData[train_ind, ]
  test <- completeData[-train_ind, ]

  result <- list("train" = train, "test" = test)

  return(result)
}

splitData <-lapply(dataXDensidad, createSetData)
trainSet<-do.call("rbind", list(splitData[[1]]$train, splitData[[2]]$train, splitData[[3]]$train))
testSet<-do.call("rbind", list(splitData[[1]]$test, splitData[[2]]$test, splitData[[3]]$test))

trainedPnn <- entrenar(trainSet)
learnPnn <- aprender(trainedPnn)
