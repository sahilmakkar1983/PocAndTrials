# generate random data in which y is a noisy function of x
x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3

# fit a linear model
res <- lm( y ~ x )
print(res)

# plot the data and the model
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
abline(res, col='blue')

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

# plot data and converging fit
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')

plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')


#Regression assignment

train = read.csv(file="C:/Users/sa313276/Documents/R programs/datasets/kc_house_train_data.csv/kc_house_train_data.csv")
test = read.csv(file="C:/Users/sa313276/Documents/R programs/datasets/kc_house_test_data.csv/kc_house_test_data.csv")

i = 1;
start=1
end=1
noOfSplits=5
df = data.frame(x = numeric(), y = numeric())
for (i in seq(1,noOfSplits)) {
  if (i == 1) {
    start = 1
    end = round(dim(train)[1]/noOfSplits)
  } else {
    start = end + 1
    end = round((dim(train)[1]*i)/noOfSplits)
  }
  data = train[start:end,]
  model <- lm(data$price ~ data$sqft_living)
  #df$x = rbind(df$x, model$coefficients[1])
  #df$y = rbind(df$y, model$coefficients[2])
  df <- rbind(df, data.frame(x = model$coefficients[1] , y = model$coefficients[2]))
}

mean(df[,1])
mean(df[,2])


train.model2 <- lm(train$price ~ train$sqft_living)
train.model3 <- lm(train$price ~ train$bedrooms)

test.model2 <- lm(test$price ~ test$sqft_living)
test.model3 <- lm(test$price ~ test$bedrooms)

summary(train.model2)
# Let's see which value can be ignored to make regression better
plot(train$sqft_living,train$price)
abline(train.model2$coefficients[1], train.model2$coefficients[2])

# Removing a value from train to improvize model
train_no13540_sqft <- train[-which(train$sqft_living == 13540),]

train_no13540_sqft.model2 <- lm(train_no13540_sqft$price ~ train_no13540_sqft$sqft_living)

#RSS
#raw data
sum((train.model2$residuals)^2)
#after removing a value
sum((train_no13540_sqft.model2$residuals)^2)

#Plot
plot(train_no13540_sqft$sqft_living,train_no13540_sqft$price,xlab="Sqft",ylab="price")
abline(train_no13540_sqft.model2$coefficients[1], train_no13540_sqft.model2$coefficients[2],col="red")


# Learning cooks distance
cook = cooks.distance(train.model2)
plot(cook)
identify(1:length(train$sqft_living),cook,train$sqft_living)

# Model using cook distance as subset
cook.minusMax5 = subset(cook,cook %in% sort(cook,decreasing=TRUE)[-1:-5])
train.model5 <- lm(train$price ~ train$sqft_living,subset =(cook < max(cook)))
train.model6 <- lm(train$price ~ train$sqft_living,subset = (cook %in% cook.minusMax5))
predict.model6= predict(train.model6, data=test)

plot(train$sqft_living,train$price)
abline(train.model6$coefficients[1], train.model6$coefficients[2],col="red")


train_multi.model2 <- lm(train$price ~ train$sqft_living + train$sqft_lot)

train_multi.model1 <- lm(train$price ~ train$sqft_living + train$bedrooms + train$bathrooms + train$sqft_lot + train$floors + train$sqft_living*train$bathrooms + train$zipcode)
predict.multi_model1= predict(train_multi.model1, data=test)
cook = cooks.distance(train_multi.model1)
cook.minusMax5 = subset(cook,cook %in% sort(cook,decreasing=TRUE)[-1])

train_multi.model3 <- lm(train$price ~ train$sqft_living + 
                           train$bedrooms + train$bathrooms + 
                           train$sqft_lot + train$floors + 
                           train$sqft_living*train$bathrooms + 
                           train$zipcode,
                           subset = (cook %in% cook.minusMax5)
                         )
predict.multi_model3= predict(train_multi.model3, data=test)

