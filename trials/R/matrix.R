# Matrix learning

m = matrix(data=c(1,2,3,4,5,6,7,8,9,10,11,12), nrow=3, ncol=4)
# Determinant is only possible for square matrix

m_sq = matrix(data=c(1,2,3,4,5,6,7,8,9,10,11,12), nrow=3, ncol=3)


#use solve() function to find inverse of matrix; doesn't work on singular matrix; 
## singular matrix is matrix whose determinant is 0

#use t() to take transpose of matrix

# Regression using matrix operations:
hsb2 = read.table("C:/Users/sa313276/Documents/R programs/hsb.txt", header=T)
y <- matrix(data=hsb2$write, ncol = 1)
y[1:10, 1]
x <- as.matrix(cbind(1, hsb2$math, hsb2$science, hsb2$socst, hsb2$female))
x[1:10,  ]

n <- nrow(x)
p <- ncol(x)

#parameter estimates
beta.hat <- solve(t(x) %*% x) %*% t(x) %*% y
beta.hat

#predicted values
y.hat <- x %*% beta.hat
y.hat[1:5, 1]

y[1:5, 1]

#the variance, residual standard error and df's
sigma2 <- sum((y - y.hat)^2)/(n - p)

#residual standard error
sqrt(sigma2)

#degrees of freedom
n - p

#the standard errors, t-values and p-values for estimates
#variance/covariance matrix
v <- solve(t(x) %*% x) * sigma2

#standard errors of the parameter estimates
sqrt(diag(v))

#t-values for the t-tests of the parameter estimates
t.values <- beta.hat/sqrt(diag(v))
t.values

#p-values for the t-tests of the parameter estimates
2 * (1 - pt(abs(t.values), n - p))

#checking that we got the correct results
ex1 <- lm(write ~ math + science + socst + female, hsb2)
summary(ex1)




#QR decomposition

A = matrix(data=c(1,1,1,1,-1,4,4,-1,4,-2,2,0), nrow=3, ncol=3)

#Calculate Norm for each column of A
A1.norm=sqrt(sum(A[,1]^2))
A2.norm=sqrt(sum(A[,2]^2))
A3.norm=sqrt(sum(A[,3]^2))

Q1=A[,1]/A1.norm
# And So on : Not doing fully

?qr.coef()
ra

#PCA and plots
sample.groups <- c(rep(1, 10), rep(2, 10), rep(3, 10),
                   rep(4, 10), rep(5, 10))
variable.groups <- c(rep(1, 10), rep(2, 10), rep(3, 10),
                     rep(4, 10), rep(5, 10), rep(6, 10),
                     rep(7, 10))

data <- matrix(nrow=length(sample.groups), ncol=70)
base.data <- matrix(nrow=length(sample.groups), ncol=7)

for (j in 1:ncol(base.data)) {
  mu <- rnorm(1, 0, 4)
  sigma <- runif(1, 5, 10)
  base.data[,j] <- sample.groups*mu +
    rnorm(length(sample.groups), 0, sigma)
}

for (j in 1:ncol(data)) {
  mu <- runif(1, 0, 4)
  data[,j] <- base.data[,variable.groups[j]] +
    rnorm(length(sample.groups), mu, 10)
}

#plot data
library(ggplot2) # for qplot
library(reshape2) #for melt function
heatmap <- qplot(x=Var1, y=Var2, data=melt(cor(data)), geom="tile",
                 fill=value)


#Starting PCA
pca <- prcomp(data, scale=T)
melted <- cbind(variable.groups, melt(pca$rotation[,1:9]))

barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value, fill=variable.groups), stat="identity") +
  facet_wrap(~Var2)

scores <- data.frame(sample.groups, pca$x[,1:3])
pc1.2 <- qplot(x=PC1, y=PC2, data=scores, colour=factor(sample.groups)) +
  theme(legend.position="none")
pc1.3 <- qplot(x=PC1, y=PC3, data=scores, colour=factor(sample.groups)) +
  theme(legend.position="none")
pc2.3 <- qplot(x=PC2, y=PC3, data=scores, colour=factor(sample.groups)) +
  theme(legend.position="none")

#NMF trials from r-blogger website
library(NMF)

# X ~ WH' 
# X is an n x p matrix
# W = n x r  user feature matrix
# H = r x p  movie feature matrix

# get ratings for 5 users on 4 movies
x1 <- c(5,4,1,1)
x2 <- c(4,5,1,1)
x3 <- c(1,1,5,5)
x4 <- c(1,1,4,5)
x5 <- c(1,1,5,4)

R <- as.matrix(rbind(x1,x2,x3,x4,x5)) # n = 5 rows p = 4 columns 

set.seed(12345)

res <- nmf(R, 2,"lee") # lee & seung method

V.hat <- fitted(res) 
print(V.hat) # estimated target matrix

w <- basis(res) #  W  user feature matrix matrix
dim(w) # n x r (n= 5  r = 4)
print(w) 

h <- coef(res) # H  movie feature matrix
dim(h) #  r x p (r = 4 p = 4)
print(h) 

# recommendor system via clustering based on vectors in H
movies <- data.frame(t(h))
features <- cbind(movies$X1,movies$X2)
plot(features)
title("Movie Feature Plot")


#NMF : Non negative matrix factorization
library("NMF")
#"Using " matrix from "Novita exampleXXXX .R"
nmfCrashlogs <- nmf(t(m), 6,"lee") # lee & seung method
