createSetData <- function(completeData){
  smp_size <- floor(0.75 * nrow(completeData))

  set.seed(123)
  train_ind <- sample(seq_len(nrow(completeData)), size = smp_size)

  x <- completeData[train_ind]
  y <- completeData[-train_ind]

  result <- list(
    "x" = x,
    "y" = y,
  )

  return(result)
}

splitData <-lapply(dataXDensidad, createSetData)
x_trainSet<-do.call("rbind", list(splitData[[1]]$x_train, splitData[[2]]$x_train, splitData[[3]]$x_train))
x_testSet<-do.call("rbind", list(splitData[[1]]$x_test, splitData[[2]]$x_test, splitData[[3]]$x_test))
y_trainSet<-c(splitData[[1]]$y_train, splitData[[2]]$y_train, splitData[[3]]$y_train)
y_testSet<-c(splitData[[1]]$y_test, splitData[[2]]$y_test, splitData[[3]]$y_test)
