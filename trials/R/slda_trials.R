library(lda)

set.seed(8675309)

args <- commandArgs(trailingOnly = TRUE)

# data file paths
trainFile <- args[1] #"reuters_2008_gics50_p1y_stockprice_lag0_duration2_polarity2pct_bow_tf_train.data"
testFile <- args[2] #"reuters_2008_gics50_p1y_stockprice_lag0_duration2_polarity2pct_bow_tf_test.data"
vocabFile <- args[3] #"reuters_2008_gics50_p1y_stockprice_lag0_duration2_polarity2pct_bow_tf_train_attributes.txt"
num.topics <- args[4] #10
num.iter <- as.integer(args[5])
predictionsFile <- args[6]
plotTopicsFile <- args[7]
plotClassesFile <- args[8]

# load vocabulary
cat("loading vocabulary...", sep="\n")
attributes <- list()
conn <- file(vocabFile, "r")
while(length(line <- readLines(conn, 1)) > 0) {
  attributes <- c(attributes, as.character(line))
}
attributes <- as.character(unlist(attributes), dim = c(dim(attributes[[1]]), length(attributes)))

# load train data and label
cat("loading training data...", sep="\n")
trainLabel <- list()
trainData <- NULL
conn <- file(trainFile, "r")
while(length(line <- readLines(conn, 1)) > 0) {
  instance <- strsplit(line, " ")
  trainLabel <- c(trainLabel, instance[[1]][1])
  
  data <- matrix(NA, nrow=2, ncol=length(instance[[1]])-1)
  for(i in 2:length(instance[[1]])) {
    pair <- strsplit(instance[[1]][i], ":")
    pair <- as.integer(unlist(pair), dim = c(2, 1))
    pair[1] <- as.integer(pair[1] - 1)  # index minus 1 to make it 0 started
    data[,i-1] <- pair
    #pair <- array(unlist(pair), dim = c(dim(pair[[1]]), length(pair[[1]])))
  }
  trainData <- c(trainData, list(data))
}
trainLabel <- array(unlist(trainLabel), dim = c(dim(trainLabel[[1]]), length(trainLabel)))

# load test data and label
cat("loading testing data...", sep="\n")
testLabel <- list()
testData <- NULL
conn <- file(testFile, "r")
while(length(line <- readLines(conn, 1)) > 0) {
  instance <- strsplit(line, " ")
  testLabel <- c(testLabel, instance[[1]][1])
  
  data <- matrix(NA, nrow=2, ncol=length(instance[[1]])-1)
  for(i in 2:length(instance[[1]])) {
    pair <- strsplit(instance[[1]][i], ":")
    pair <- as.integer(unlist(pair), dim = c(2, 1))
    pair[1] <- as.integer(pair[1] - 1)  # index minus 1 to make it 0 started
    data[,i-1] <- pair
    #pair <- array(unlist(pair), dim = c(dim(pair[[1]]), length(pair[[1]])))
  }
  testData <- c(testData, list(data))
}
testLabel <- array(unlist(testLabel), dim = c(dim(testLabel[[1]]), length(testLabel)))


# start slda procedure

## Initialize the params
params <- sample(c(-1, 1), num.topics, replace=TRUE)

cat("training slda...", sep="\n")
result <- slda.em(documents=trainData,
                  K=num.topics,
                  vocab=attributes,
                  num.e.iterations=num.iter, #50,
                  num.m.iterations=num.iter, #4,
                  alpha=1.0, eta=0.1,
                  trainLabel,
                  params,
                  variance=0.25,
                  lambda=1.0,
                  logistic=FALSE,
                  method="sLDA")

## Make a pretty picture.
require("ggplot2")
Topics <- apply(top.topic.words(result$topics, 10, by.score=TRUE),
                2, paste, collapse=" ")
coefs <- data.frame(coef(summary(result$model)))
theme_set(theme_bw())
coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))
coefs <- coefs[order(coefs$Estimate),]
jpeg(filename=plotTopicsFile, width=1440, height=900)
qplot(Topics, Estimate, colour=Estimate, size=abs(t.value), data=coefs) +
  geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error,
                               ymax=Estimate+Std..Error)) + coord_flip()
dev.off()

cat("predicting...", sep="\n")
predictions <- slda.predict(testData,
                            result$topics, 
                            result$model,
                            alpha = 1.0,
                            eta=0.1)
write(predictions, file=predictionsFile, sep="\n")

jpeg(filename=plotClassesFile, width=1440, height=900)
qplot(predictions,
      fill=factor(testLabel),
      xlab = "predicted rating",
      ylab = "density",
      alpha=I(0.5),
      geom="density") +
  geom_vline(aes(xintercept=0)) +
  opts(legend.position = "none")
dev.off()


#### we don't need the rest
#predicted.docsums <- slda.predict.docsums(trainData,
#                                          result$topics, 
#                                          alpha = 1.0,
#                                          eta=0.1)
#predicted.proportions <- t(predicted.docsums) / colSums(predicted.docsums)
#
#qplot(`Topic 1`, `Topic 2`, 
#      data = structure(data.frame(predicted.proportions), 
#                       names = paste("Topic", 1:10)), 
#      size = `Topic 3`)
####