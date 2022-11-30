library(lattice)
library(ggplot2)
library(e1071) 
library(zoo)
library(tidyverse)
library(tensorflow)
library(keras)

#install_tensorflow()
#use_condaenv("r-tensorflow")

#set.seed(2022)
#--------------------------------------------------------------------------------------------------------------------------------------

Data0<-read.csv('weeklydata.csv')
names(Data0)



# "wsloss"     "wsncl"      "wspcp"      "wspcp_lag1" "wspcp_lag2" "wmpcp1"     "wmpcp_lag1"
##########################################################
########################################################

wsloss<-Data0$wsloss/1000
wsncl<-Data0$wsncl 

wspcp<-Data0$wspcp
wspcp_lag1<-Data0$wspcp_lag1
wspcp_lag2<-Data0$wspcp_lag2
wmpcp1<-Data0$wmpcp1
wmpcp_lag1<-Data0$wmpcp_lag1


###########################################################################################

y0<-wsncl
x0<- data.frame(wspcp,wspcp_lag1,wmpcp1)


n11<-1:length(wsncl) 
d1=data.frame(wsncl,wspcp,wspcp_lag1,wmpcp1)

dataset<-data.frame(wsncl,wspcp,wspcp_lag1,wmpcp1)
head(dataset)



d1=data.frame(wsncl,wspcp,wspcp_lag1,wmpcp1)
dim(d1)

#--------------------- GA SVR ------------------------------
#x0<- data.frame(wspcp,wspcp_lag1,wmpcp1)
#cost   gamma   epsilon # 23.09179 0.1157251 0.3349581

model <- svm(x0,y0, cost =23.09179, gamma =0.1157251 , epsilon =  0.3349581, type = "eps-regression", kernel = "radial")

y02<- predict(model, x0)



#######################New data ##############################################

d5<- environment()
load("City_A_NACORDEX.RData")
#CLM1 : HadGEM2 ES 8.5 

Ystart <- seq(2021, 2071, by = 10)
YtoSelect <- cbind(Ystart, Ystart + 9)

##########################################################################
#--------------------CLM3:  GFDL ESM2MR 8.5  ----------------------------------------------
Data <- d5$pr.rcp85.GFDL.ESM2M.RegCM4.day.NAM.22$D
Data$Year <- as.numeric(format(Data$Date, "%Y"))

# t=1 --- 2021 2030
rpcp1 <- Data$vardata[Data$Year>=YtoSelect[1,1] & Data$Year <= YtoSelect[1,2] ]
x11<-rollapply(rpcp1, 7, sum, by = 7)

m2<-512
wspcp1<-x11[3:m2];      # pcp-weakly sum 
wspcp2<-x11[2:(m2-1)];  #  lag 1
wspcp3<-x11[1:(m2-2)];  #  lag 2

wmpcp_b<-rollapply(rpcp1, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


xCLM3<-data.frame(wspcp1,wspcp2,wmpcp22)
dim(xCLM3)


# Prediction ========================
y11<-predict(model,xCLM3, type="raw")## 
y11[y11<0]<-0
length(y11)

plot.ts(y11,xaxt="n",col="black",lwd=2,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(),main="")


####################################################################################
####################### Sieve Bootstrap - #####################################

#-------------------------Diagnostics  --------------------
residual<-(y0- y02);summary(residual)

par(mfrow=c(3,2))
library(tseries)
plot.ts(residual,col="black",xlab="",ylab="residuals",main="")

plot(residual)

acf(residual)
pacf(residual)


adf.test(residual)
#Augmented Dickey-Fuller Test

#data:  residual
#Dickey-Fuller = -6.3587, Lag order = 7, p-value = 0.01
#alternative hypothesis: stationary


# Test for Hetrosecdasticity 
plot (y02,residual,xlab="Fitted values",ylab="Residual",main="Fitted vs Residual")


#-------Sieve Bootstrap -------------------

err<-(y0- y02)  # residual of the original fitted model

library(forecast)

auto.arima(err) 
z1<-arima(err,order=c(1,1,1))
rs<-z1$residuals
summary(rs)

z<-ar(err) 
summary(z)
rs2<-z$resid
summary(rs2[-1])




mu_fit3<-sum(y02)/10; mu_fit3 
mu_obs<-sum(y0)/10; mu_obs


pred_loss<-y02

rep=100
ln<-length(y0);ln
m5<-dim(xCLM3)[1];m5

pred11<-c();pred12<-c();pred13<-c();pred14<-c();pred15<-c();pred16<-c()


#==================================================================================

for(i in 1:rep){  # i=1                     # Number of bootstrap sample
  r<-sample(rs,ln, replace = TRUE)          # subsample from original 'err' set 
  y_b=pred_loss+r                           # New (independent) yi
  
  #y0<-wsncl
  #x0<- data.frame(wspcp,wspcp_lag1)
  

  #============================================================================
  #============================================================================
  
  model2 <- svm(x0,y_b, cost =23.09179, gamma =0.1157251 , epsilon =  0.3349581, type = "eps-regression", kernel = "radial")

  #========================================================================
  #========================================================================  
  
   pred2<-predict(model2,xCLM3, type="raw")## 
   pred11<-c(pred11,pred2)
  
  
  
  
}


pred1<-pred11
summary(pred1)

#pred1[pred1<0]<-0  
mat<-matrix(pred1,m5,rep, byrow=T)
dim(mat) # 510 100

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

LL1<-0
UL1<-0

for (j in 1:m5){
  
  LL<-quantile(mat[j,],c(0.025)) # 95% LL for each point
  UL<-quantile(mat[j,],c(0.975)) # 95%  UL for each point
  

  
  LL1<-c(LL1,LL[[1]])    
  UL1<-c(UL1,UL[[1]])    
  
}

LL<-LL1[-1]   # vector of LL of all data points sequentially
UL<-UL1[-1]   # vector of UL of all data points sequentially



################################################################
######################## Plots ###############################


library(tseries)

plot.ts(y11,xaxt="n",col="black",lwd=2,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(-1,20),main="")
#lines(pred,col="red" )
lines(UL,col="green")
lines(LL,col="blue")

legend(par('usr')[4],cex=0.7,
       c('Observed', 'Bootstrap UL','Bootstrap LL'),
       lwd=c(1,1,1), 
       lty=c(1,1,1),
       col=c('black', 'green','blue'))

#################################################################################

plot(y11, type="n", ylim=range(0,UL),xlab="Year",
     ylab="Number of claims",  xaxt="n",first.panel=grid())

polygon(c(time(y11),rev(time(y11))), c(UL,rev(LL)), 
        col=rgb(0,0,0.9,0.4), border=FALSE)

lines(y11,lwd=1,col="black")


axis(1, at=c(1,   105,  209,  313,417,510),
     labels=c("2020", "2022", "2024", "2026","2028","2030"))


# Figure width-830, height-379


###########################Save and Import Prediction ####################################


NCL_Pred<-data.frame(yCLM5,UL,LL)

A_NCL_predicted_Clim5 <- read.csv("A_NCL_predicted_Clim5.csv")
A_NCL_predicted_Clim5 <- read.csv("A_NCL_predicted_Clim5.csv")

y11<-A_NCL_predicted_Clim5$y11
UL<-A_NCL_predicted_Clim5$UL
LL<-A_NCL_predicted_Clim5$LL

library(tseries)

plot.ts(y11,xaxt="n",col="black",lwd=2,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(),main="City A (4.5), ")
#lines(pred,col="red" )
lines(UL,col="green")
lines(LL,col="blue")

legend(par('usr')[4],cex=0.7,
       c('Observed', 'Bootstrap UL','Bootstrap LL'),
       lwd=c(1,1,1), 
       lty=c(1,1,1),
       col=c('black', 'green','blue'))



plot(y11, type="n", ylim=range(0,UL),xlab="Year (2021-2030)",
     ylab="NCL", main="City A (CLM5: CAN 4.5)",first.panel=grid())

polygon(c(time(y11),rev(time(y11))), c(UL,rev(LL)), 
        col=rgb(0,0,0.9,0.4), border=FALSE)

lines(y11,lwd=1)





