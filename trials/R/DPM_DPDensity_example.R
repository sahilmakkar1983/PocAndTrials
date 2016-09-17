# Data
data(airquality)
attach(airquality)
library(ggplot2)
library(MASS)
library(nlme)
library(survival)
library(splines)
library(DPpackage)
ozone <- Ozone**(1/3)
radiation <- Solar.R
# Prior information
s2 <- matrix(c(10000,0,0,1),ncol=2)
m2 <- c(180,3)
psiinv2 <- solve(s2)
prior <- list(alpha=1,a0=1,b0=1/5,
              psiinv2=psiinv2,nu1=4,nu2=4,s2=s2)
              

#prior <- list(a0=1,b0=1/5,nu1=4,nu2=4,s2=s2,
#              #m2=m2,psiinv2=psiinv2,tau1=0.01,tau2=0.01)
#              m2=m2,tau1=0.01,tau2=0.01)

# Initial state
state <- NULL
# MCMC parameters
nburn <- 5000
nsave <- 10000
nskip <- 10
ndisplay <- 1000
mcmc <- list(nburn=nburn,nsave=nsave,nskip=nskip,ndisplay=ndisplay)
# Fit the model
fit1 <- DPdensity(y=cbind(radiation,ozone),prior=prior,mcmc=mcmc,
                  state=state,status=TRUE,na.action=na.omit)

#trying out Tivo logs(m comes from filename "novita_logs_XXXX")
fit2 <- DPdensity(y=cbind(m),prior=prior,mcmc=mcmc,
                  state=state,status=TRUE,na.action=na.omit)

# Plot the estimated density
plot(fit1)
# Extracting the density estimate
fit1$x1
fit1$x2
fit1$dens
## End(Not run)
#qplot(radiation, ozone, colour = fit1$state$ss, 
#      data = na.omit(cbind(radiation,ozone)))

qplot(radiation[which((complete.cases(cbind(radiation,ozone)) ))],
      ozone[which((complete.cases(cbind(radiation,ozone)) ))], 
      colour = as.factor(fit1$state$ss),ylab = "Ozone", xlab="radiation")


data(calgb)
attach(calgb)
y <- cbind(Z1,Z2,Z3,T1,T2,B0,B1)
x <- cbind(CTX,GM,AMOF)
z <- cbind(y,x)
# Data for prediction
data(calgb.pred)
xpred <- as.matrix(calgb.pred[,8:10])
# Prior information
prior <- list(pe1=0.1,
              pe0=0.1,
              ae=1,
              be=1,
              a0=rep(1,3),
              b0=rep(1,3),
              nu=12,
              tinv=0.25*var(z),
              m0=apply(z,2,mean),
              S0=var(z),
              nub=12,
              tbinv=var(z))
# Initial state
state <- NULL

# MCMC parameters
mcmc <- list(nburn=5000,
             nsave=5000,
             nskip=3,
             ndisplay=100)
# Fitting the model
fit1 <- HDPMcdensity(formula=y~x,
                     study=~study,
                     xpred=xpred,
                     prior=prior,
                     mcmc=mcmc,
                     state=state,
                     status=TRUE)
# Posterior inference
fit1
summary(fit1)
# Plot the parameters
# (to see the plots gradually set ask=TRUE)
plot(fit1,ask=FALSE)
# Plot the a specific parameters
# (to see the plots gradually set ask=TRUE)
plot(fit1,ask=FALSE,param="eps",nfigr=1,nfigc=2)
# Plot the measure for each study
# under first values for the predictors, xpred[1,]
predict(fit1,pred=1,i=1,r=1) # pred1, study 1
predict(fit1,pred=1,i=2,r=1) # pred1, study 2
# Plot the measure for each study
# under second values for the predictors, xpred[2,]
predict(fit1,pred=2,i=1,r=1) # pred2, study 1
predict(fit1,pred=2,i=2,r=1) # pred2, study 2
# Plot the idiosyncratic measure for each study
# under first values for the predictors, xpred[1,]
predict(fit1,pred=1,i=1,r=0) # study 1
predict(fit1,pred=1,i=2,r=0) # study 2
# Plot the common measure
# under first values for the predictors, xpred[1,]
predict(fit1,pred=1,i=0)
## End(Not run)




## DIRCHILET VARIANT OF HDP
## Not run:

library(ggplot2)
library(MASS)
library(nlme)
library(survival)
library(splines)
library(DPpackage)
# Data
data(calgb)
attach(calgb)
y <- cbind(Z1,Z2,Z3,T1,T2,B0,B1)
# Prior information
prior <- list(pe1=0.1,
              pe0=0.1,
              ae=1,
              be=1,
              a0=rep(1,3),
              b0=rep(1,3),
              nu=9,
              tinv=0.25*var(y),
              m0=apply(y,2,mean),
              S0=var(y),
              nub=9,
              tbinv=var(y))
# Initial state
state <- NULL
# MCMC parameters
mcmc <- list(nburn=5000,
             nsave=5000,
             nskip=3,
             ndisplay=100)
# Fitting the model
fit1 <- HDPMdensity(y=y,
                    study=study,
                    prior=prior,
                    mcmc=mcmc,
                    state=state,
                    status=TRUE)
# Posterior inference
fit1
summary(fit1)
# Plot the parameters
# (to see the plots gradually set ask=TRUE)
plot(fit1,ask=FALSE)
# Plot the a specific parameters
# (to see the plots gradually set ask=TRUE)
plot(fit1,ask=FALSE,param="eps",nfigr=1,nfigc=2)
# Plot the measure for each study
predict(fit1,i=1,r=1) # study 1
predict(fit1,i=2,r=1) # study 2
# Plot the idiosyncratic measure for each study
predict(fit1,i=1,r=0) # study 1
predict(fit1,i=2,r=0) # study 2
# Plot the common measure
predict(fit1,i=0)
## End(Not run)



## Not run:
# Data
data(galaxy)
galaxy<-data.frame(galaxy,speeds=galaxy$speed/1000)
attach(galaxy)
# Initial state
state <- NULL
# MCMC parameters
nburn<-300
nsave<-3000
nskip<-3
ndisplay<-30
mcmc <- list(nburn=nburn,nsave=nsave,nskip=nskip,ndisplay=ndisplay)
# Prior
prior<-list(aa0=2.01,
            ab0=0.01,
            kmax=1000,
            a0=1,
            b0=1)
# Fitting the model
fit <- BDPdensity(y=speeds,prior=prior,mcmc=mcmc,
                  state=state,status=TRUE)
plot(fit)




library(dpmixsim)
data("galaxy")
x0 <- galaxy$speed
x <- prescale(x0)
maxiter <- 4000; rec <- 3000; ngrid <- 100
res <- dpmixsim(x, M=1, a=1, b=0.1, upalpha=1, maxiter=maxiter, rec=rec,
                nclinit=4)
z <- postdpmixciz(x=x, res=res, rec=rec, ngrid=ngrid, plot=T)
##

maxiter <- 200; rec <- 150; ngrid <- 10
res <- dpmixsim(m[1:5,], M=2, a=1, b=0.001, upalpha=0, maxiter=maxiter,
                rec=rec, nclinit=4)
z <- postdpmixciz(m[1:5,], res=res, rec=rec, ngrid=ngrid, plot=T)
##-----------------
## Example 2:
demo(testMarronWand)
##-----------------
## Example 3: MRI segmentation
## Testing note: this example should reproduce the equivalent segmented
## images used in the author's references
slicedata <- readsliceimg(fbase="t1_pn3_rf0", swap=FALSE)
image(slicedata$niislice, col=gray((0:255)/256), main="original image")
x0 <- premask(slicedata, subsamp=TRUE)
x <- prescale(x0)
rec <- 3000
res <- dpmixsim(x, M=1, a=1, b=1, upalpha=1, maxiter=4000,
                rec=rec, nclinit=8, minvar=0.002)
## post-simulation
ngrid <- 200
z <- postdpmixciz(x, res=res, rec=rec, ngrid=ngrid, plot=TRUE)
x0 <- premask(slicedata, subsamp=FALSE) # use full-sized image after estimation
x <- prescale(x0)
cx <- postdataseg(x, z, ngrid=ngrid)
cat("*** view grouped segmentations:\n")
postimgclgrp(slicedata$mask, cx, palcolor=FALSE)
cat("*** display all clusters:\n")
postimgcomps(slicedata$mask, cx)
cat("*** re-cluster with 4 clusters:\n")
postkcluster(slicedata$mask, cx, clk=4)