
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

##########################################################################################

plot.ts(wsloss,col="black")
lines(wspcp,col='red')

###########################################################################################
y0<-wsloss
#x0<- data.frame(wspcp,wspcp_lag1,wsncl)
#x0<- data.frame(wspcp,wspcp_lag1,wmpcp1)           #loss: 29,647, val_loss: 84,138
#x0<- data.frame(wspcp,wspcp_lag1)                  #loss: 29,421, val_loss: 84,113 
#x0<- data.frame(wspcp,wspcp_lag1,wmpcp1)            #loss: 29,210, val_loss: 84,443 

#x0<- data.frame(wsncl,wspcp,wspcp_lag1,wmpcp1)   # rmse  34.50272


##########Final Model ##################
x0<- data.frame(wsncl,wspcp,wspcp_lag1)   #loss: 1,284 val_loss: 915.1 

#x0<- data.frame(wsncl,wspcp,wspcp_lag1,wspcp_lag2) #     loss: 1,343 val_loss: 969.2 


#############################################################################################
#------------Correlation---------------------------
library(Hmisc)
rcorr(as.matrix(x0), type = c("pearson"))


n11<-1:length(wsncl) 
d1=data.frame(wsloss,wsncl,wspcp,wspcp_lag1,wmpcp1)
head(d1)

dataset<-data.frame(wsloss,wsncl,wspcp,wspcp_lag1,wmpcp1)
head(dataset)


#===============================================================

lapply(dataset, function(x) sum(is.na(x))) %>% str()
dataset <- na.omit(dataset)



###### Split the data into training and test sets######
#######Split features from labels#############################
#Separate the target value-the "label"-from the features. 
#This label is the value that you will train the model to predict.
# wsncl,wspcp,wspcp_lag1

library(rsample)
split <- initial_split(dataset, 0.8)
train_dataset <- training(split)
test_dataset <- testing(split)



#ALL_train_features <- dataset %>% select(-wsloss)
#dim(ALL_train_features)
#ALL_train_labels <- dataset %>% select(wsloss)
#dim(ALL_train_labels)


train_features <- train_dataset %>% select(-wsloss)
test_features <- test_dataset %>% select(-wsloss)

train_labels <- train_dataset %>% select(wsloss)
test_labels <- test_dataset %>% select(wsloss)



## Normalization
# It is good practice to normalize features that use different scales and ranges.
# The Normalization layer
#The first step is to create the layer:

normalizer <- layer_normalization(axis = -1L)
normalizer %>% adapt(as.matrix(train_features))
print(normalizer$mean)


#########################################################################################
## Deep Neural Network (DNN)----Medium 
## Control Overfit with L2 regularization and dropout ###

#=============================================================================

build_and_compile_model <- function(norm) {
  model <- keras_model_sequential() %>%
    norm() %>%
    
    layer_dense(64, kernel_regularizer = regularizer_l2(0.0001),activation = 'elu') %>%
    layer_dropout(0.2) %>%
    
    layer_dense(64, kernel_regularizer = regularizer_l2(0.0001),activation = 'elu') %>%
    layer_dropout(0.2) %>%
    
    layer_dense(64, kernel_regularizer = regularizer_l2(0.0001),activation = 'elu') %>%
    layer_dropout(0.2) %>%

    layer_dense(1)
  
  model %>% compile(
    loss = 'mse', # mse # mean_absolute_error
    optimizer = optimizer_adam(0.001)
  )
  
  model
}

#mean_squared_error
#========================================================================

dnn_model <- build_and_compile_model(normalizer)
summary(dnn_model)


# Use Keras Model.fit to execute the training for 10 epochs:
history <- dnn_model %>% fit(
  as.matrix(train_features),
  as.matrix(train_labels),
  validation_split = 0.2,
  verbose = 0,
  epochs = 100
)

### Loss 
history
plot(history)



##################Train data comparison ##########################

y02 <- predict(dnn_model, as.matrix(x0))
#summary(y02)
rmse <- sqrt(mean((y0-y02) ^ 2));rmse #34.53047



y02[y02<0]<-0
plot.ts(y0, col="black", lwd=1,ylim=c(0,600), first.pannel=grid())
lines(y02, col="blue", lwd=1)

legend("topleft",cex=0.7,
       c('Fitted', 'Observed'),
       lwd=c(1,1), 
       lty=c(1,1),
       col=c('blue', 'black'))



###########################################################
################# New data(PCP) 2021-2030 #############################################

############### Future No of Claims from NCL model #####################
City_A_Claim_fit <- read.csv("City_A_Claim_fit.csv")
City_A_Claim_fit <- read.csv("City_A_Claim_fit.csv")

 

############### CLM5: CAN 4.5 #########################################


rpcp<-read.csv("A_rcp45_2021_2080.csv")[,2] #pcp
length(rpcp)

# 2021-2030: Projected NCL for diffrent 10 years periods

pcp_21_30_r=rpcp[1:3650]
x11<-rollapply(pcp_21_30_r, 7, sum, by = 7) 
length(x11)

#m<-length(x11)
m2<-512

wspcp1<-x11[3:m2];      # pcp-weakly sum 
wspcp2<-x11[2:(m2-1)];  #  lag 1
wspcp3<-x11[1:(m2-2)];  #  lag 2

wmpcp_b<-rollapply(pcp_21_30_r, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


wsncl2<-City_A_Claim_fit$yCLM5

xCLM5<-data.frame(wspcp1,wspcp2,wspcp3,wmpcp22) #,wsncl2
head(xCLM5)

dim(xCLM5)

# Prediction ========================
yCLM5 <- predict(dnn_model, as.matrix(xCLM5)) ## Change here 
#y11

yCLM5[yCLM5<0]<-0
length(yCLM5)

plot.ts(yCLM5,xaxt="n",col="black",lwd=2,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(),main="City A (4.5), NN")


s1=sum(yCLM5)/10 # 2021-2030
mu_fit1<-sum(y02)/10; mu_fit1 # 2010-2020

rf11_CLM5<-((s1/mu_fit1)-1)*100      # 
rf11_CLM5




################ CLM6: CAN 8.5 #################################

rpcp<-read.csv("A_rcp85_2021_2080.csv")[,2] #pcp

# 2021-2030: Projected NCL for diffrent 10 years periods

pcp_21_30_r=rpcp[1:3650]
x11<-rollapply(pcp_21_30_r, 7, sum, by = 7);
#m<-length(x11)

#wsncl2<-City_A_Claim_fit$yCLM6


m2<-512
wspcp1<-x11[3:m2];      # pcp-weakly sum 
wspcp2<-x11[2:(m2-1)];  #  lag 1
wspcp3<-x11[1:(m2-2)];  #  lag 2

wmpcp_b<-rollapply(pcp_21_30_r, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


xCLM6<-data.frame(wspcp1,wspcp2,wspcp3,wmpcp22)

dim(xCLM6)

# Prediction ========================
yCLM6 <- predict(dnn_model, as.matrix(xCLM6)) ## Change here 


yCLM6[yCLM6<0]<-0
length(yCLM6)

plot.ts(y11,xaxt="n",col="black",lwd=2,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(),main="City A (4.5), NN")


s1=sum(yCLM6)/10 # 2021-2030
mu_fit1<-sum(y02)/10; mu_fit1 # 2010-2020

rf11_CLM6<-((s1/mu_fit1)-1)*100      # 
rf11_CLM6

################################################################################# 
#################### New data 2 ##############################################


d5<- environment()
load("City_A_NACORDEX.RData")
#CLM1 : HadGEM2 ES 8.5 
#CLM2: MPI ESM 8.5    
#CLM3:  GFDL ESM2MR 8.5  
#CLM4: GFDL ESM2MW 8.5 

Ystart <- seq(2021, 2071, by = 10)
YtoSelect <- cbind(Ystart, Ystart + 9)

#########################################################################
#--------------------------CLM1 : HadGEM2 ES 8.5  ---------------------------------------------------------
# Projected NCL for diffrent 10 years periods --------------------------------------
Data <- d5$pr.rcp85.HadGEM2.ES.RegCM4.day.NAM.22$D
Data$Year <- as.numeric(substr(Data$Date, start=1, stop=4))

# t=1 --- 2021 2030
rpcp1 <- Data$vardata[Data$Year>=YtoSelect[1,1] & Data$Year <= YtoSelect[1,2] ]
x11<-rollapply(rpcp1, 7, sum, by = 7)
#m2<-length(x11) #m2 #521

wsncl2<-City_A_Claim_fit$yCLM1


m2<-512
wspcp1<-x11[3:m2];      # pcp-weakly sum 
wspcp2<-x11[2:(m2-1)];  #  lag 1
wspcp3<-x11[1:(m2-2)];  #  lag 2

wmpcp_b<-rollapply(pcp_21_30_r, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


xCLM1<-data.frame(wspcp1,wspcp2,wspcp3,wmpcp22)
dim(xCLM1)

# Prediction ========================
yCLM1 <- predict(dnn_model, as.matrix(xCLM1))
yCLM1[yCLM1<0]<-0
length(yCLM1)



s1=sum(yCLM1)/10 # 2021-2030
mu_fit1<-sum(y02)/10; mu_fit1 # 2010-2020

rf11_CLM1<-((s1/mu_fit1)-1)*100      # 
rf11_CLM1


#########################################################################
#--------------------CLM2: MPI ESM 8.5   --------------------------------------------------------------- 

Data <- d5$pr.rcp85.MPI.ESM.LR.RegCM4.day.NAM.22$D
Data$Year <- as.numeric(format(Data$Date, "%Y"))


# t=1 --- 2021 2030
rpcp1 <- Data$vardata[Data$Year>=YtoSelect[1,1] & Data$Year <= YtoSelect[1,2] ]
x11<-rollapply(rpcp1, 7, sum, by = 7)

m2<-512
wspcp1<-x11[3:m2];      # pcp-weakly sum 
wspcp2<-x11[2:(m2-1)];  #  lag 1
wspcp3<-x11[1:(m2-2)];  #  lag 2

wmpcp_b<-rollapply(pcp_21_30_r, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]

xCLM2<-data.frame(wspcp1,wspcp2,wspcp3,wmpcp22)
dim(xCLM2)

# Prediction ========================
yCLM2 <- predict(dnn_model, as.matrix(xCLM2))
yCLM2[yCLM2<0]<-0
length(yCLM2)



s1=sum(yCLM2)/10 # 2021-2030
mu_fit1<-sum(y02)/10; mu_fit1 # 2010-2020

rf11_CLM2<-((s1/mu_fit1)-1)*100      # 
rf11_CLM2




#########################################################################
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

wmpcp_b<-rollapply(pcp_21_30_r, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]

#wsncl2<-City_A_Claim_fit$yCLM3

xCLM3<-data.frame(wspcp1,wspcp2,wspcp3,wmpcp22)
dim(xCLM3)

# Prediction ========================
yCLM3 <- predict(dnn_model, as.matrix(xCLM3))
yCLM3[yCLM3<0]<-0
length(yCLM3)



s1=sum(yCLM3)/10 # 2021-2030
mu_fit1<-sum(y02)/10; mu_fit1 # 2010-2020

rf11_CLM3<-((s1/mu_fit1)-1)*100      # 
rf11_CLM3


#########################################################################
#--------------------CLM4: GFDL ESM2MW 8.5 ----------------------------------------------
Data <- d5$pr.rcp85.GFDL.ESM2M.WRF.day.NAM.22$D
Data$Year <- as.numeric(format(Data$Date, "%Y"))

# t=1 --- 2021 2030
rpcp1 <- Data$vardata[Data$Year>=YtoSelect[1,1] & Data$Year <= YtoSelect[1,2] ]
x11<-rollapply(rpcp1, 7, sum, by = 7)

m2<-512
wspcp1<-x11[3:m2];      # pcp-weakly sum 
wspcp2<-x11[2:(m2-1)];  #  lag 1
wspcp3<-x11[1:(m2-2)];  #  lag 2

wmpcp_b<-rollapply(pcp_21_30_r, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]

#wsncl2<-City_A_Claim_fit$yCLM4


xCLM4<-data.frame(wspcp1,wspcp2,wspcp3,wmpcp22)
dim(xCLM4)

# Prediction ========================
yCLM4 <- predict(dnn_model, as.matrix(xCLM4))
yCLM4[yCLM4<0]<-0
length(yCLM4)



s1=sum(yCLM4)/10 # 2021-2030
mu_fit1<-sum(y02)/10; mu_fit1 # 2010-2020 

rf11_CLM4<-((s1/mu_fit1)-1)*100      # 
rf11_CLM4

## % change in no. of claim in 2020-2030  ############################
## Uncertainty for different climate model ##

City_A_Claim_2020_2030<-rbind(rf11_CLM1,rf11_CLM2,rf11_CLM3,rf11_CLM4,rf11_CLM5,rf11_CLM6)
City_A_Claim_2020_2030


year<-c('21-30', '31-40', '41-50', '51-60', '61-70','71-80')
tab1<-City_A_Claim_2020_2030[,1]

as.numeric(City_A_Claim_2020_2030[,1])



barplot(as.numeric(City_A_Claim_2020_2030[,1]), #beside=T,
        col=c("black", "blue", "green", "red", "gray","gold3"), 
        ylab="Change in annual aggregate loss, %",
        xlab="2021-2030",
        las=1,
        main="",
        legend = c("CanESM2 4.5", "CanESM2 8.5", "MPI ESM 8.5",
                   "GFDL ESM2MR 8.5","GFDL ESM2MW 8.5", "HadGEM2 ES 8.5"), 
        args.legend = list(title = "Climate scenario", x = "topright", cex = 0.70),
        ylim = c(0, 40), first.panel=grid())
#grid()
box()


 
 



## plot=================================

plot.ts(yCLM1,col="black",xlab="Year",ylab="",ylim=c(), type ="l")
lines(yCLM2, col="blue",  type ="l")
lines(yCLM3, col="red",  type ="l")
lines(yCLM4, col="green",  type ="l")




