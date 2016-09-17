library(tm)

## get a sample (10 documents) of the Reuters dataset (comes with package tm)
#reut21578 <- system.file("texts", "crude", package = "tm")
#
#reuters <- Corpus(DirSource(reut21578), 
#	readerControl = list(reader = readReut21578XML))


### download reuters21578 data first (use first 1000 documents; 1984/85)
#file <- "reut2-000.xml" 
#reuters <- Corpus(ReutersSource(file), readerControl = list(reader = readReut21578XML))
logs= read.csv("C:/Users/sa313276/Documents/R programs/datasets/100_lines_after_and_before_EOBT.csv",
               header=TRUE)
reuters <- Corpus(VectorSource(logs$text))


reuters
reuters[[1]][1]

## Convert to Plain Text Documents
#reuters <- tm_map(reuters, as.PlainTextDocument)
#reuters[[1]][1]

## Convert to Lower Case
reuters <- tm_map(reuters, tolower)
reuters[[1]][1]

## Remove all library names
reuters <- tm_map(reuters, function(x) {
  PlainTextDocument(
    #gsub('[[:xdigit:]]+', ' ', x)
    #gsub("^lib+", ' ', x)
    gsub("lib\\w+ *.so", ' ', x)
    #gsub('[[:xdigit:]]+ ', ' ', x)
  )
})
reuters[[1]][1]


## Remove Numbers
#reuters <- tm_map(reuters, removeNumbers)
## Remove Punctuations
#reuters <- tm_map(reuters, removePunctuation)
reuters <- tm_map(reuters, function(x) {
  PlainTextDocument(
    #gsub('[[:xdigit:]]+', ' ', x)
    gsub('[[:punct:]]+', ' ', x)
    #gsub('[[:xdigit:]]+ ', ' ', x)
  )
})
reuters[[1]][1]

reuters <- tm_map(reuters, function(x) {
  PlainTextDocument(
    #gsub('[[:xdigit:]]+', ' ', x)
    gsub('0x[[:xdigit:]]+ ', ' ', x)
    #gsub('[[:xdigit:]]+ ', ' ', x)
  )
})
reuters[[1]][1]

reuters <- tm_map(reuters, function(x) {
  PlainTextDocument(
    #gsub('[[:xdigit:]]+', ' ', x)
    gsub('[[:digit:]]+ ', ' ', x)
    #gsub('[[:xdigit:]]+ ', ' ', x)
  )
})
reuters[[1]][1]

## Remove Stopwords
reuters <- tm_map(reuters, removeWords, stopwords("english"))
reuters[[1]][1]

## Remove Punctuations
#reuters <- tm_map(reuters, removePunctuation)
#reuters[[1]][1]

## Stemming
#reuters <- tm_map(reuters, stemDocument)
#reuters[[1]][1]

reuters <- tm_map(reuters, function(x) {
  PlainTextDocument(
    #gsub('[[:xdigit:]]+', ' ', x)
    gsub("error |fatal |kernel |tmk |harmony | activity| read", ' ', x)
    #gsub('[[:xdigit:]]+ ', ' ', x)
  )
})
reuters[[1]][1]


## Eliminating Extra White Spaces
reuters <- tm_map(reuters, stripWhitespace)
reuters[[1]][1]

## create a term document matrix
#dtm <- DocumentTermMatrix(reuters, 
#                          control = list(
#                            weighting = function(x) 
#                              weightTfIdf(x,normalize = FALSE)
#                            )
#)
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[1:10, 1:100])

findFreqTerms(dtm, 100)
findAssocs(dtm, "activity", .4)
#washington  secretari  political     reagan republican      white      regan 
#      1.00       0.49       0.46       0.45       0.45       0.42       0.41 
#staff strategist 
#0.41       0.41 



## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10, 1001:1010])

#to see top 100 terms in dtm
dtm_tfxidf$dimnames$Terms[dtm_tfxidf$j[order(dtm_tfxidf$v)[0:100]]]

#Word Cloud trials
library(RColorBrewer)
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
wordVectorWithHighTfIdfForEachDoc = c()
print dtm_tfxidf$nrow
for (i in 1:dtm_tfxidf$nrow) {
  start=(((i-1)*dtm_tfxidf$nrow)+1)
  end=i*dtm_tfxidf$nrow
#  print(start)
#  print(end)
#  print(dtm_tfxidf$v[start:end])
  wordVectorWithHighTfIdfForEachDoc = c(wordVectorWithHighTfIdfForEachDoc,
                                        dtm_tfxidf$j[order(dtm_tfxidf$v[start:end],decreasing = TRUE)[1:2]]
                                        #order(dtm_tfxidf$v[start:end],decreasing = TRUE)[1:2]]
                                        )
  #print(dtm_tfxidf$dimnames$Terms[dtm_tfxidf$j[order(dtm_tfxidf$v[start:end],decreasing = TRUE)[1:5]]])
}
#wordVectorWithHighTfIdfForEachDoc = dtm_tfxidf$j[wordVectorWithHighTfIdfForEachDoc]
#wordcloud(dtm_tfxidf$dimnames$Terms[dtm_tfxidf$j[order(dtm_tfxidf$v)]],dtm_tfxidf$v[dtm_tfxidf$j[order(dtm_tfxidf$v)]],max.words = 40)
wordcloud(dtm_tfxidf$dimnames$Terms[wordVectorWithHighTfIdfForEachDoc],dtm_tfxidf$v[wordVectorWithHighTfIdfForEachDoc],max.words = 40)
## do document clustering

### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)


### cluster into 10 clusters
cl <- kmeans(m, 10)
cl

clNo8 = which(cl$cluster[] == 8)

table(cl$cluster)

### show clusters using the first 2 principal components
#plot(prcomp(m)$x, col=cl$cl)
plot(dtm_tfxidf, col=cl$cl)

findFreqTerms(dtm[cl$cluster==1], 50)
inspect(reuters[which(cl$cluster==1)])

## hierarchical clustering
library(proxy)

### this is going to take 4-ever (O(n^2))
d <- dist(m, method="cosine")
hc <- hclust(d, method="average")
plot(hc)
str(hc)

clCutree <- cutree(hc, 50)
table(clCutree)
findFreqTerms(dtm[clCutree==1], 50)


#LDA trials
library(lda)
