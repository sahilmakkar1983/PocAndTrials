
sales = c(2323,323,3232,3232,5456,7567,878,243,767,979,3232,64565,8787)
boxplot(sales)

require(utils)
data()                         # list all available data sets
try(data(package = "rpart") )  # list the data sets in the rpart package
data(USArrests, "VADeaths")    # load the data sets 'USArrests' and 'VADeaths'
## Not run: ## Alternatively
ds <- c("USArrests", "VADeaths"); data(list = ds)
## End(Not run)
help(USArrests)   


data() # lists all the datasets in all the packages in memory
data(package="datasets") # lists all the datasets in the "datasets" package
data(Orange) # loads the orange dataset in memory
?Orange # Help for the "Orange" Datasets
str("datasets") # gives the structure of all the datasets in the datasets package.

boxplot(AirPassengers)
a= AirPassengers
boxplot(a)
plot(a)

b= co2
plot(b)

c=HairEyeColor
head(c)
c
plot(c)

d= Titanic
fix(d)

e=USArrests
e

#Package
library(MASS)
help(package = MASS)
ls("package:MASS")

a = c(10000,8000,11000,13000,14000,25000)
meanA = mean(a)
sdA = sd(a)
zTest = (a - meanA)/sdA

p_yellow1 <- pnorm(a[1], meanA, sdA)    #using x, mu, and sigma
p_yellow2 <- pnorm(zTest[1])#using z-score of 2.107


p_blue1 <- 1 - p_yellow1   #using x, mu, and sigma
p_blue2 <- 1 - p_yellow2   #using z-score of 2.107

x <- seq(-4, 4, length=100)
dx <- dnorm(x) # dnorm determines the height(y), given a poing within given distribution
px = pnorm(x) # pnrom determines probability, given a point within given distribution
qx = qnorm(.05,0,1) # qnorm determines the horizontal axis point(x), given probability for distribution 


#Interquartile range
mydata = c(1,2,5,6,7,9,12,15,18,19,27)

#Lower point of IQR
lowerQuartileEnd=quantile(mydata,1/4)
#Upper point of IQR
upperQuartileStart=quantile(mydata,3/4)
#Center line in IQR
IQR(mydata) # internally IQR is calculated using upperQuartileStart-lowerQuartileEnd


#First dummy project

videoDisplayTime = seq(1,2,length=10000)


#Lets add some outliers to our Data
###################################
videoDisplayTime = union(videoDisplayTime,seq(3.5,10,length=3))
#Lower point of IQR
lowerQuartileEnd=quantile(videoDisplayTime,1/4)
#Upper point of IQR
upperQuartileStart=quantile(videoDisplayTime,3/4)
#Center line in IQR
IQR(videoDisplayTime) # internally IQR is calculated using upperQuartileStart-lowerQuartileEnd
#Outlier logic
lowerOutliers=IQR(videoDisplayTime) -1.5*lowerQuartileEnd
upperOutliers=IQR(videoDisplayTime) +1.5*upperQuartileStart
boxplot(videoDisplayTime)

#Lets get values of higher video display time as upperOutliers 
videoDisplayTime[which(videoDisplayTime > upperOutliers)]

