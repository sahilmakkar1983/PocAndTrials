# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

nOfSeries=100
x <- seq(-4, 4, length=nOfSeries)



#Calculating p-value using z-score
##First Way
value=3
z = value-mean(x)/(sd(x)/sqrt(nOfSeries))
2*pnorm(-abs(z))
##Second Way
2*(1-pnorm(value,mean=mean(x),sd=sd(x)/sqrt(nOfSeries)))

#Calculating p-value from t-distribution : CROSS VERIFY, this formula might be wrong
t = value-mean(x)/(sd(x)/sqrt(nOfSeries))
2*pt(-abs(t),df=nOfSeries-1)
pt(x,df=nOfSeries-1)

plot(x)

#t.test
t.test(x)
y = c(9.0,9.5,9.6,10.2,11.6)
hy <- dnorm(y)
plot(y,hy)
t.test(y)
t.test(y, mu=5)
t.test(y, mu=10)
t.test(y, mu=9.98)


plot(x,pt(x,df=nOfSeries-1))
plot(x,dnorm(x))
plot(x,pnorm(x))
