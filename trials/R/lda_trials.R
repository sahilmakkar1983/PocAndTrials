library("NLP")
library("lda")
library("tm")


string_1 = "Hello vegetables fruits  good  health"
string_2 = "Cars crazy, drive"
string_3 = "Music love romance date"
string_4 = "Music drive"
string_5 = "Travel guide australia america jojo"

documents_corpus = list(string_1=string_1,string_2=string_2,
                        string_3=string_3,string_4=string_4,string_5=string_5)

set.seed(20)
lexiclaized = lexicalize(doclines = documents_corpus)
lda_model = lda.collapsed.gibbs.sampler(lexiclaized$documents,vocab = lexiclaized$vocab, 
                      K = 3, num.iterations =  5000,alpha = .1, eta=.1)

#print.lda(visualize.2.topic(lexiclaized,lda_model$assignments))
#print()

predict_lda=predictive.distribution(lda_model$document_sums,lda_model$topics,.1,.1)

test_document_corpus = list(string_1="hello vegetable good")
test_document_corpus = lexicalize(doclines = test_document_corpus  )
#predict_lda=predictive.distribution(lda_model$document_sums,lda_model$topics,.1,.1)




# Second trial on LDA (after long time: date 25-May-2016)
library(tm)

#crashLogs <- Corpus(crashLogsSource(file), readerControl = list(reader = readReut21578XML))
logs= read.csv("C:/Users/sa313276/Documents/R programs/datasets/100_lines_after_and_before_EOBT.csv",
               header=TRUE)
crashLogs <- Corpus(VectorSource(logs$text))

## Convert to Plain Text Documents
#crashLogs <- tm_map(crashLogs, as.PlainTextDocument)
#crashLogs[[1]][1]

## Convert to Lower Case
crashLogs <- tm_map(crashLogs, tolower)

## Remove all library names
crashLogs <- tm_map(crashLogs, function(x) {
  PlainTextDocument(
    gsub("lib\\w+ *.so", ' ', x)
  )
})
#crashLogs[[1]][1]

## Remove Punctuations
#crashLogs <- tm_map(crashLogs, removePunctuation)
crashLogs <- tm_map(crashLogs, function(x) {
  PlainTextDocument(
    gsub('[[:punct:]]+', ' ', x)
  )
})

#Remove Numbers
crashLogs <- tm_map(crashLogs, function(x) {
  PlainTextDocument(
    gsub('0x[[:xdigit:]]+ ', ' ', x)
  )
})
crashLogs <- tm_map(crashLogs, function(x) {
  PlainTextDocument(
    gsub('[[:digit:]]+ ', ' ', x)
  )
})
#TBD : this two removals need extra check later
crashLogs <- tm_map(crashLogs, function(x) {
  PlainTextDocument(
    gsub('0x\\w+', ' ', x)
  )
})
crashLogs <- tm_map(crashLogs, function(x) {
  PlainTextDocument(
    gsub('0\\w+', ' ', x)
  )
})


## Remove Stopwords
crashLogs <- tm_map(crashLogs, removeWords, stopwords("english"))
#crashLogs[[1]][1]

## Stemming
#crashLogs <- tm_map(crashLogs, stemDocument)
#crashLogs[[1]][1]

#crashLogs <- tm_map(crashLogs, function(x) {
#  PlainTextDocument(
#gsub("error |fatal |kernel |tmk |harmony | activity| read", ' ', x)
#  )
#})
#Remove more irrelevant words
crashLogs <- tm_map(crashLogs, removeWords, c(' error ', ' fatal',' kernel', 
                                              ' tmk',' harmony',' activity',
                                              'read',' build',' killed',
                                              ' signal ','timestamp ',
                                              'dump','kernel','stack',
                                              'thread','log','txt',
                                              'tivo','address'))

## Eliminating Extra White Spaces
crashLogs <- tm_map(crashLogs, stripWhitespace)
crashLogs<-tm_map(crashLogs,removeWords,c("\t"))
crashLogs[[1]][1]

## create a term document matrix
#dtm <- DocumentTermMatrix(crashLogs, 
#                          control = list(
#                            weighting = function(x) 
#                              weightTfIdf(x,normalize = FALSE)
#                            )
#)
dtm <- DocumentTermMatrix(crashLogs)
freq<-colSums(as.matrix(dtm))   

print(names(freq))


ord<-order(freq,decreasing=TRUE)

write.csv(freq[ord],"word_freq.csv")

#load topic models library
library(topicmodels)


#Set parameters for Gibbs sampling
burnin<-400
iter<-200
thin<-50
seed<-list(2003,5,63,100001,765)
nstart<-5
best<-TRUE


#Number of topics
k<-6

#Run LDA using Gibbs sampling
#ldaOut=LDA(dtm,k,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin = burnin,iter = iter,thin=thin))
perplexityV2=c()
for (k in seq(5,160)){
  print(Sys.time())
  print(k)
  ldaOut=LDA(dtm,k,method="Gibbs",control=list(alpha=1,keep=100))
  perplexityV2=c(perplexityV2,perplexity(ldaOut,dtm))
}


#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))


#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
