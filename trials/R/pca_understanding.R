#data
x=c(2.5,0.5 ,2.2 ,1.9 ,3.1 ,2.3 ,2 ,1 ,1.5 ,1.1)
y=c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6,1.1,1.6,0.9)

#mean
mean_x=mean(x)
mean_y=mean(y)

x_minus_mean = x - mean_x
y_minus_mean = y - mean_y

#covariance matrix
cov_matrix = matrix(nrow=2, ncol=2)
cov_matrix[1,1] = cov(x,x)
cov_matrix[2,2] = cov(y,y)
cov_matrix[1,2] = cov(x,y)
cov_matrix[2,1] = cov(y,x)

#calculate eigen-value and eigenvectors
eigen_values = eigen(cov_matrix)$values
eigen_vector = eigen(cov_matrix)$vector
