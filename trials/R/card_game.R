a = read.csv("C:/Users/sa313276/Documents/R programs/cards.csv",header=T)
a[2,]

deal <-function(){
  card = a[sample(1:dim(a)[1],replace=T)[1],]
  #card= sample(c(a[2,]),replace=T)
  return(card)
}

card=deal();
card

#Few more trials
Red_Card <- subset(a, a$Color == "Red")
Red_Card

?sample
sample(1:dim(a)[1])
dim(a)
a[1,]


?rnorm
#Extra learning
rn <- rnorm(1000, 5)
plot(rn)
boxplot(rn)
quantile(replicate(1000, mean(sample(rn, replace = TRUE))),
         probs = c(0.025, 0.975))
t.test(rn)$conf.int
