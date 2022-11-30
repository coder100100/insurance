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


DD<-data.frame(wsncl,wsloss,wspcp,wspcp_lag1,wspcp_lag2,wmpcp1)

library("Hmisc")
r1 = rcorr(as.matrix(DD),type=c("spearman")) #,"spearman"
r1




par(mfrow=c(3,1))

plot.ts(wspcp,xaxt="n",col="black",lwd=1,xlab="",
        ylab="Precipitation (mm)/week",xlim=c(),
        ylim=c(),main="", first.panel=grid())


plot.ts(wsncl,xaxt="n",col="black",lwd=1,xlab="",
        ylab="Number of claims/week",xlim=c(),
        ylim=c(),main="", first.panel=grid())


plot.ts(wsloss,xaxt="n",col="black",lwd=1,xlab="Year",
        ylab="Amount of loss (K)/week",xlim=c(),
        ylim=c(),main="", first.panel=grid())

axis(1, at=c( 1,  103, 205,307, 409, 508),
     labels=c("2002", "2004", "2006", "2008","2010","2012"))



###########################################################################################

y0<-wsncl


#x0<- data.frame(wspcp,wspcp_lag1) # 3.039917
#cost   gamma   epsilon # 57.1462 0.2082679 0.3358885

#### Final Model #####################################################
x0<- data.frame(wspcp,wspcp_lag1,wmpcp1)

# 2.981616
#cost   gamma   epsilon # 23.09179 0.1157251 0.3349581
####################################################################

#x0<- data.frame(wspcp,wspcp_lag1,wspcp_lag2) # 3.058846
#cost   gamma   epsilon # 6.201953 0.12322 0.8709106

#x0<- data.frame(wspcp,wspcp_lag1,wspcp_lag2,wmpcp1) # 2.992643
#cost   gamma   epsilon # 6.943104 0.10821 0.7999719



###################### Genetic Algorithm (GA) #############################################

#data22<-data.frame(y0,wspcp,wspcp_lag1)

#source("./Insurance Cliam model/Models weekly data V_4/All City R codes/Genetic_algorithm.R")
#GA<-G_Algorithm(data22,5)
#GA

############################################################################################

#--------------------- GA SVR ------------------------------

model <- svm(x0,y0, cost =23.09179, gamma =0.1157251 , epsilon =  0.3349581, type = "eps-regression", kernel = "radial")

pred<- predict(model, x0)
rmse <- sqrt(mean((y0-pred) ^ 2));rmse #2.981616




plot.ts(y0, col="black", lwd=1,ylim=c(0,30), first.pannel=grid())
lines(pred, col="blue", lwd=1)

legend("topleft",cex=0.7,
       c('Fitted', 'Observed'),
       lwd=c(1,1),
       lty=c(1,1),
       col=c('blue', 'black'))

# residual of the original fitted model
E<-(y0- pred)
summary(E)
summary(pred)

length(pred)


# Sum/10 of observed NCL and fitted NCL
mu_fit1<-sum(pred)/10; mu_fit1
mu_obs<-sum(wsncl)/10; mu_obs




###########################################################
################# New data(PCP) 2021-2030 #############################################

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


xCLM5<-data.frame(wspcp1,wspcp2,wmpcp22)
head(xCLM5);dim(xCLM5)

# Prediction ========================
yCLM5<-predict(model,xCLM5, type="raw")

yCLM5[yCLM5<0]<-0
length(yCLM5)

plot.ts(yCLM5,xaxt="n",col="black",lwd=1,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(),main="City A (4.5), NN")


s1=sum(yCLM5)/10 # 2021-2030

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

xCLM6<-data.frame(wspcp1,wspcp2,wmpcp22)
dim(xCLM6)

# Prediction ========================
yCLM6<-predict(model,xCLM6, type="raw")##
#y11

yCLM6[yCLM6<0]<-0
length(yCLM6)

plot.ts(yCLM6,xaxt="n",col="black",lwd=1,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(),main="City A (4.5), NN")


s1=sum(yCLM6)/10 # 2021-2030

rf11_CLM6<-((s1/mu_fit1)-1)*100      #
rf11_CLM6




############### CLM7: rcp45.GFDL-ESM2M.CRCM5 #########################################

rpcp<-read.csv("rcp45.GFDL-ESM2M.CRCM5_2021.csv")[,5] #pcp
length(rpcp)
head(rpcp)

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


xCLM7<-data.frame(wspcp1,wspcp2,wmpcp22)
dim(xCLM7)


head(xCLM7)
dim(xCLM7)


# Prediction ========================
yCLM7<-predict(model,xCLM7, type="raw")##
#y11

yCLM7[yCLM7<0]<-0
length(yCLM7)


plot.ts(yCLM7,xaxt="n",col="black",lwd=2,xlab="Year (2021-2030)",ylab="NCL",
        ylim=c(),main="City A (4.5), NN")


s1=sum(yCLM7)/10 # 2021-2030

rf11_CLM7<-((s1/mu_fit1)-1)*100      #
rf11_CLM7






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



m2<-512
wspcp1<-x11[3:m2];      # pcp-weakly sum
wspcp2<-x11[2:(m2-1)];  #  lag 1
wspcp3<-x11[1:(m2-2)];  #  lag 2

wmpcp_b<-rollapply(rpcp1, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


xCLM1<-data.frame(wspcp1,wspcp2,wmpcp22)
dim(xCLM1)

# Prediction ========================
yCLM1<-predict(model,xCLM1, type="raw")##
yCLM1[yCLM1<0]<-0
length(yCLM1)

s1=sum(yCLM1)/10 # 2021-2030
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

wmpcp_b<-rollapply(rpcp1, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


xCLM2<-data.frame(wspcp1,wspcp2,wmpcp22)
dim(xCLM2)

# Prediction ========================
yCLM2<-predict(model,xCLM2, type="raw")##
yCLM2[yCLM2<0]<-0
length(yCLM2)

s1=sum(yCLM2)/10 # 2021-2030
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

wmpcp_b<-rollapply(rpcp1, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


xCLM3<-data.frame(wspcp1,wspcp2,wmpcp22)
dim(xCLM3)

# Prediction ========================
yCLM3<-predict(model,xCLM3, type="raw")##
yCLM3[yCLM3<0]<-0
length(yCLM3)

s1=sum(yCLM3)/10 # 2021-2030
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

wmpcp_b<-rollapply(rpcp1, 7, max, by = 7);
wmpcp22<-wmpcp_b[3:m2]


xCLM4<-data.frame(wspcp1,wspcp2,wmpcp22)
dim(xCLM4)

# Prediction ========================
yCLM4<-predict(model,xCLM4, type="raw")##
yCLM4[yCLM4<0]<-0
length(yCLM4)



s1=sum(yCLM4)/10 # 2021-2030

rf11_CLM4<-((s1/mu_fit1)-1)*100      #
rf11_CLM4


###################################################################################################
############# Predicted number of claims ###########################################################

ncl_predicted<-data.frame(yCLM1,yCLM2,yCLM3,yCLM4,yCLM5,yCLM6,yCLM7)
head(ncl_predicted)

dim(ncl_predicted)


#write.csv(ncl_predicted,"./Insurance Cliam model/Models weekly data V_4/All City R codes/Outputs 2022/NCL_City_A_Claim_fit_SVR.csv")
#write.csv(ncl_predicted,"./Insurance Cliam model/Models weekly data V_4/All City R codes/Outputs 2022/NCL_City_A_Claim_fit_SVR.csv")



plot.ts(yCLM6[159:211],xaxt="n",col="gold3",lwd=1,xlab="Year",
        ylab="Number of claims",xlim=c(0,52),
        ylim=c(0,20),main="", first.panel=grid())

lines(yCLM5[159:211], col='gray')
lines(yCLM7[159:211], col='brown')

lines(yCLM1[159:211], col='black')
lines(yCLM2[159:211], col='blue')
lines(yCLM3[159:211], col='green')
lines(yCLM4[159:211], col='red')




#=================================================

BMA_Canada <- read.csv("BMA_Canada.csv")

Rensem<-(yCLM6[107:263]+yCLM5[107:263]+yCLM7[107:263]+yCLM1[107:263]+yCLM2[107:263]+yCLM3[107:263]+yCLM4[107:263])/7


plot.ts(yCLM6[107:263],xaxt="n",col="gold3",lwd=1,xlab="Year",
        ylab="Number of claims",xlim=c(0,162),
        ylim=c(0,18),main="", first.panel=grid())

lines(yCLM5[107:263], col='chocolate4')
lines(yCLM7[107:263], col='brown')

lines(yCLM1[107:263], col='gray')
lines(yCLM2[107:263], col='red')
lines(yCLM3[107:263], col='green')
lines(yCLM4[107:263], col='gray20')

lines(Rensem, col='black',lwd=2,lty=1)
lines(BMA_Canada$BMA_NCL[107:263], col='blue',lwd=2)

axis(1, at=c(1 ,  52,  105,  157),
     labels=c("2023", "2024", "2025", "2026"))

legend("topright", cex=0.65,
       c("CanESM2 4.5", "CanESM2 8.5", "MPI ESM 8.5",
         "GFDL ESM2MR 8.5","GFDL ESM2MW 8.5", "HadGEM2 ES 8.5",
         "GFDL ESM2M 4.5","Raw ensemble","BMA"),
       lwd=c(1,1,1,1,1,1,1,2,2), lty=c(1,1,1,1,1,1,1,1,1),
       col=c("gray", "red", "green", "gray20", "chocolate4","gold3",
             "brown",'black','blue'))

# Figure width-830, height-379



## % change in no. of claim in 2020-2030  ############################
## Uncertainty for different climate model ##

City_A_Claim_2020_2030<-rbind(rf11_CLM1,rf11_CLM2,rf11_CLM3,rf11_CLM4,rf11_CLM5,rf11_CLM6,rf11_CLM7)
City_A_Claim_2020_2030


year<-c('21-30', '31-40', '41-50', '51-60', '61-70','71-80')
tab1<-City_A_Claim_2020_2030[,1]

as.numeric(City_A_Claim_2020_2030[,1])

barplot(as.numeric(City_A_Claim_2020_2030[,1]), #beside=T,
        col=c("black", "blue", "green", "red", "gray","gold3","brown"),
        ylab="Change in annual number of claims, %",
        xlab="2021-2030",
        las=1,
        main="",
        legend = c("CanESM2 4.5", "CanESM2 8.5", "MPI ESM 8.5",
                   "GFDL ESM2MR 8.5","GFDL ESM2MW 8.5", "HadGEM2 ES 8.5", 'GFDL ESM2M 4.5'),
        args.legend = list(title = "Climate scenario", x = "topright", cex = 0.65),
        ylim = c(0, 25))
#grid()
box()


#####################################################################################
############ Barplot for raw ensemble and BMA #####################


BMANCL<-BMA_Canada$BMA_NCL
Rensem<-(yCLM6+yCLM5+yCLM7+yCLM1+yCLM2+yCLM3+yCLM4)/7



s_BMA=sum(BMANCL)/10 # 2021-2030
rf11_BMA<-((s_BMA/mu_fit1)-1)*100      #
rf11_BMA # 11.48279


s_Raw=sum(Rensem)/10 # 2021-2030
rf11_RawEnm<-((s_Raw/mu_fit1)-1)*100      #
rf11_RawEnm  # 11.64248

rBMA_loss=19.63053
rRaw_loss=6.623146


BMA_Raw<-c(rf11_BMA,rf11_RawEnm,rBMA_loss,rRaw_loss)
BMA_Raw


data <- data.frame(values = c(11.482791, 11.642485, 19.630530,  6.623146),  # Create example data
                   group = rep(c("Number of claims",
                                 "Aggregate loss"),
                               each = 2),
                   subgroup = c("BMA","Raw Ensemble"))


data_base <- reshape(data,                        # Modify data for Base R barplot
                     idvar = "subgroup",
                     timevar = "group",
                     direction = "wide")
row.names(data_base) <- data_base$subgroup
data_base <- data_base[ , 2:ncol(data_base)]
colnames(data_base) <- c("Number of claims",
                        "Aggregate loss")
data_base <- as.matrix(data_base)
data_base



barplot(height = data_base,
        beside = TRUE,
        col=c("black", "blue"),
        ylab="Change in annual claims, %",
        xlab="2021-2030",
        las=1,
        main="",
        legend = c("BMA", 'Raw Ensemble'),
        args.legend = list(x = "topleft", cex = 0.75),
        ylim = c(0, 25))
box()




#################################################################################################






