library(pnn)

data(norms)
pnn <- learn(norms)
pnn <- smooth(pnn, sigma=0.5)
guess(pnn, c(1,2))

testData <- completeData[2:5];
smp_size <- floor(0.75 * nrow(testData))

set.seed(123)
train_ind <- sample(seq_len(nrow(testData)), size = smp_size)

x <- testData[train_ind, ]
y <- t(do.call("rbind", testData[-train_ind, 2:4]))

result <- list(
  "x" = x,
  "y" = y
)

pnn <- learn(result$x)
pnn <- smooth(pnn, sigma=0.1)
guess(pnn, c(95.0000,19.0000,1.6111 ))
