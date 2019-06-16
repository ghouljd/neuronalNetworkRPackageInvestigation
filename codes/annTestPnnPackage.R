createSetData <- function(completeData){
  smp_size <- floor(0.75 * nrow(completeData))

  set.seed(123)
  train_ind <- sample(seq_len(nrow(completeData)), size = smp_size)

  x_train <- completeData[train_ind, 2:4]
  x_test <- completeData[-train_ind, 2:4]

  y_train <- completeData[train_ind, 1]
  y_test <- completeData[-train_ind, 1]

  result <- list(
    "x_train" = x_train,
    "y_train" = y_train,
    "x_test" = x_test,
    "y_test" = y_test
  )

  return(result)
}

splitData <-lapply(dataXDensidad, createSetData)
x_trainSet<-do.call("rbind", list(splitData[[1]]$x_train, splitData[[2]]$x_train, splitData[[3]]$x_train))
x_testSet<-do.call("rbind", list(splitData[[1]]$x_test, splitData[[2]]$x_test, splitData[[3]]$x_test))
y_trainSet<-c(splitData[[1]]$y_train, splitData[[2]]$y_train, splitData[[3]]$y_train)
y_testSet<-c(splitData[[1]]$y_test, splitData[[2]]$y_test, splitData[[3]]$y_test)

y_trainSet<-replace(y_trainSet, y_trainSet==2 | y_trainSet==1, 0)
y_trainSet<-replace(y_trainSet, y_trainSet==3, 1)
y_testSet<-replace(y_testSet, y_testSet==2 | y_testSet==1, 0)
y_testSet<-replace(y_testSet, y_testSet==3, 1)
