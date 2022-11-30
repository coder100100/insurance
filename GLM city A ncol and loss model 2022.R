
library(lattice)
library(ggplot2)
library(e1071) 
library(zoo)

library(tidyverse)
library(tensorflow)
library(keras)

#install_tensorflow()
#use_condaenv("r-tensorflow")

set.seed(2022)
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

#### Final Model 
x0<- data.frame(wspcp,wspcp_lag1,wmpcp1)


################### glm - Poisson Regression #################################

############################## NCL Model####################################################

model_glm<-glm(y0~wspcp+wspcp_lag1+wmpcp1, family = poisson)

pred<-predict(model_glm, x0, type="response")
plot.ts(pred)

rmse <- sqrt(mean((y0-pred) ^ 2));rmse 


############################## Loss Model ####################################################

y0<-wsloss 
y0[y0==0]<-001 # replace zero with very small number
x0<- data.frame(wsncl,wspcp,wspcp_lag1,wmpcp1)

model_glm2<-glm(y0~wsncl+wspcp+wspcp_lag1+wmpcp1, family = Gamma(link=log), maxit = 1000)

predL<-predict(model_glm2, x0, type="response")
plot.ts(predL,ylim=c(0,1000))

rmse <- sqrt(mean((y0-predL) ^ 2));rmse   









