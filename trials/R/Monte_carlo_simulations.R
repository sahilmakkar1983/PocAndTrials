n <- 10000
f <- function(x) x^2
plot(runif(n), runif(n), col='blue', pch=20)
curve(f, 0,1, n=100, col='white', add=TRUE)

ps <- matrix(runif(2*n), ncol=2)
g <- function(x,y) y <= x^2
z <- g(ps[,1], ps[,2])
plot(ps[!z,1], ps[!z,2], col='blue', pch=20)
points(ps[z,1], ps[z,2], col='green', pch=20)
curve(f, 0,1, n=100, col='white', add=TRUE)
      
?runif
?points


# Fresh trial
#MCMC from URL : http://nicercode.github.io/guides/mcmc/

#1 Monte carlo sample generations
m <- 0
s <- 1

set.seed(1)
samples <- rnorm(10000, m, s)
#Samples can be called as sample generate for monte carlo, 
#these are generally calculated with bayessian stats primarily on posterior prob distributions

#Explore
summary(samples)
plot(summary(samples))

summary(replicate(1000, mean(rnorm(10000, m, s))))

# Monte carlo integration
p <- 0.025 # 2.5 %, 95% confidence interval
a.mc <- unname(quantile(samples,p))

cummeans <- function(x)
  cumsum(x) / seq_along(x)


plot(cummeans(samples), 
     type='l', 
     xlab="sample", ylab="Cummulative mean", 
     panel.first = abline(h=0, col="red"), las=1, log="x")
# see log="x" this is converted to log() here
for (i in seq_len(30))
  lines(cummeans(rnorm(10000,m,s)),
        col=rgb(runif(1), runif(1),runif(1),0.5))
# How many times you do Monte carlo, results converge to same is show in plot


a.true <- qnorm(p,m,s) # qnorm gives point on x axis, in this case 
a.true

#Error between true and MC
a.true - a.mc
#Observe error

#a.mc generate multiple times using replicate function
a.mc1 <- replicate(100, mean(rnorm(10000, m, s)))
a.true - a.mc1
summary(a.true - a.mc1)
#Printing a.true - a.mc1 would show that results are almost same; 
#it means mean of rnorm(10000,m,s) converges to single solution

#TIPS : learning(need to confirm) till now; 
#as I remember we have to sample 10000 times from normal distribution and consider mean of each sampling


##UNDERSTANDING Markov chains
# Markov chains : given a current state we can to next state
# It follows to stationary distribution where next state doesn't change

#Let's assume probability transition matrix
p <- rbind(c(.5,.25,.25),
           c(.2,.1,.7),
           c(.25,.25,.5))

rowSums(p) # notice all row sums to 1
colSums(p) # Columns may not NECESSARILY sum to 1

iterate.p <- function(x, P, n) {
  res <- matrix(NA, n+1, length(x)) # To store current state to next 'n' states
  res[1,] <- x
  for (i in seq_len(n))
    res[i+1,] <-x <- x %*% P
  res
}

n <- 10
y1 <- iterate.p(c(1,0,0),p,n)

y2 <- iterate.p(c(0,1,0),p,n)
y3 <- iterate.p(c(0,0,1),p,n)

matplot(0:n,y1, type='l', xlab = "step", ylab = "y", las=1)
matlines(0:n,y2, lty=2)
matlines(0:n,y3, lty=3)
#All graph above proves that markov chains converge to stationary distribution

#Trying same with eigen vector/values
v <- eigen(t(p), FALSE)$vectors[,1] # this code-stmt would draw only 1st vector, generally for this eigen value is '1'
v <- v/sum(v)

#use v (eigen vector for eigen value 1)
points(rep(10,3),v, col=1:3)


#A step further : TBD
run <- function(i, P ,n)  {
  res = integer(n)
  for (t in seq_len(n))
    res[[t]] <-i <- sample(nrow(P),1,pr=P[i,])
  res
}

samples <- run(1,p,100)
plot(samples,type='s',xlab = "steps", ylab = "state", las=1)

#plot means rather
plot(cummeans(samples == 1),type="l",ylim = c(0,1), 
     xlab = "steps", ylab = "y", las=1)
lines(cummeans(samples == 2), col=2)
lines(cummeans(samples == 3), col=3)


#Run same for 5000 iterations
samples <- run(1,p,5000)
#plot(samples,type='s',xlab = "steps", ylab = "state", las=1, log="x")

#plot means rather
plot(cummeans(samples == 1),type="l",ylim = c(0,1), 
     xlab = "steps", ylab = "y", las=1)
lines(cummeans(samples == 2), col=2)
lines(cummeans(samples == 3), col=3)
abline(h=v,lty=2, col=1:3)

#Let's implement MCMC now
# Let's consider target distribution
p = .4
mu = c(-1,2)
sd = c(.5,2)
targetDist <- function(x) {
  p * dnorm(x,mu[1], sd[1]) +
    (1-p) * dnorm(x,mu[2], sd[2]) 
}

#Trying x from -4 to 8 with 301 samples - just observe curve
curve(targetDist(x), col="red", -4,8,n=301,las=1)
# las=1 makes sure that all y axis symbols are there

# Now understand, we have to take samples from current-point=mean with some sd
# Please note that putting x as new mean on each iteration is representation for markov chain
# rnorm is nothing but monte-carlo simulation step
q = function(x) rnorm(1,x,4)

#MCMC core algorithm
step <- function(x, f, q) {
  xp = q(x)
  alpha = min(1, f(xp)/f(x)) # remember f(xp) is driven from previous sample as mean
  #cat ("old-value", x,"\n")
  #cat ("new-value", xp,"\n")
  #if new value better than previous than SURELY consider new value, 
  #even if new value is NOT better than prev, we may consider new value with probability.
  if (runif(1) < alpha)
    x = xp
  x
}

# Run function for MCMC algorithm
run <- function(x, f, q, nsteps) {
  resM <- matrix(NA, nsteps, length(x))
  for (i in seq_len(nsteps)) {
    x <- step(x,f,q)
    #cat ("step-out",x,"\n")
    resM[i,] <- x
  }
  
  drop(resM)
}

res <- run(-10,targetDist,q,1000)


#Let's draw and explorative analyze
layout(matrix(c(1,2),1,2),widths = c(4,1))
par(mar=c(4.1,.5,.5,.5), oma=c(0,4.1,0,0))
plot(res,type="s", xpd=NA, ylab="parameter",xlab = "Sample",las=1)
usr <-par("usr")
xx <- seq(usr[3], usr[4],length=301)
plot(targetDist(xx),xx,type="l",yaxs="i",axes=FALSE,xlab="")

hist(res,50,freq=FALSE,main="",ylim =c(0,.4),las=1,xlab="x",ylab="prob density")
z <- integrate(targetDist,-Inf, Inf)$value
curve(targetDist(x)/z,add=TRUE,col="red",n=200)

#Run MCMC with different combos
  res.fast <- run(-10,targetDist,function(x) rnorm(1,x,33),1000)
  res.slow <- run(-10,targetDist,function(x) rnorm(1,x,.3),1000)
  #Draw
  layout(matrix(c(1,2),1,2),widths = c(4,1))
  par(mar=c(4.1,.5,.5,.5), oma=c(0,4.1,0,0))
  plot(res,type="s", xpd=NA, ylab="parameter",xlab = "Sample",las=1)
  lines(res.fast,col="red")
  lines(res.slow,col="green")
  plot(targetDist(xx),xx,type="l",axes=FALSE)
  
  
  