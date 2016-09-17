##########################################################
### --- Text Mining with R - Text Classification
##########################################################
# load packages
library("plyr")
library("tm")
library("class")
##########################################################
##########################################################
##########################################################
# define options
options(stringsAsFactors = FALSE)

# set parameters
candidates <- c("romney", "obama")
pathname <- "C:\\Users\\sa313276\\Documents\\R programs\\datasets\\speeches"
# clean texts
cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
  return(corpus.tmp)
}
# create text document matrix
generateTDM <- function(cand, path){
  s.dir <- sprintf("%s\\%s", path, cand)
  s.cor <- Corpus(DirSource(directory = s.dir, encoding = "UTF-8"))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = cand, tdm= s.tdm)
}
# execute function and create a Text Document Matrix
tdm <- lapply(candidates, generateTDM, path = pathname)
# inspect results
str(tdm)

# attach names of candidates
bindCandidatetoTDM <- function(tdm){
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
}
# apply function
candTDM <- lapply(tdm, bindCandidatetoTDM)
# inspect data
str(candTDM)
# inspect obama corpus
# inspect(tdm[[2]]$tdm)

# stack texts
tdm.stack <- do.call(rbind.fill, candTDM)
tdm.stack[is.na(tdm.stack)] <- 0
# inspect data
head(tdm.stack)

# create hold-out
# Divide training and test data
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7))
test.idx <- (1:nrow(tdm.stack))[-train.idx]# create model - knn clustering

tdm.cand <- tdm.stack[, "targetcandidate"]
tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% "targetcandidate"]
# set up model
knn.pred <- knn(tdm.stack.nl[train.idx,], tdm.stack.nl[test.idx,], tdm.cand[train.idx])
# determine accuracy
conf.mat <- table("Predictions" = knn.pred, Actual = tdm.cand[test.idx])
# calculate accuracy
(accuracy <- sum(diag(conf.mat)) / length(test.idx) * 100)

#plot(knn.pred)



#26th-May-2016, trying same with LDA
#load text mining library
library(tm)
