
library(ddalpha)
library(DepthProc)
library(zoo)
library(ineq)






setwd("./Insurance Cliam model/Models weekly data V_4/All City R codes/Data Depth/")

source('scaleCurve_AD.R')
source('ddPlot_AD.R')



##############################################City A NCL###################################################


setwd("./Insurance Cliam model/Models weekly data V_4/All City R codes/Data Reults/Prediction_Loss/") # set working directory
#setwd("C:/Users/akd130230/OneDrive/Insurance Cliam model/Models weekly data V_4/All City R codes/Data Reults/Prediction_Loss/") # set working directory

d1<-read.csv("A_Loss_predicted_Clim1.csv")
d2<-read.csv("A_Loss_predicted_Clim2.csv")
d3<-read.csv("A_Loss_predicted_Clim3.csv")
d4<-read.csv("A_Loss_predicted_Clim4.csv")



#===============================================
#nobs=513
nobs=500  #
##################################################################################################
#------------------------2021.2030----------------------------------

u1<-cbind(d1$ncl_2021.2030[1:nobs],d2$ncl_2021.2030[1:nobs],d3$ncl_2021.2030[1:nobs],d4$ncl_2021.2030[1:nobs])
dpA2021.2030<- depth(u1,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv1<-rev((1/dpA2021.2030) -1)
v1<-scaleCurve_AD(u1,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))


##########################################################################
#--------------------------City C-------------------------------
#--------------------------------------

setwd("./Insurance Cliam model/Models weekly data V_4/All City R codes/Data Reults/Prediction_Loss/") # set working directory


d1<-read.csv("C_Loss_predicted_Clim1.csv")
d2<-read.csv("C_Loss_predicted_Clim2.csv")
d3<-read.csv("C_Loss_predicted_Clim3.csv")
d4<-read.csv("C_Loss_predicted_Clim4.csv")



##################################################################################################
#------------------------2021.2030----------------------------------

u0C<-cbind(d1$ncl_2021.2030[1:nobs],d2$ncl_2021.2030[1:nobs],d3$ncl_2021.2030[1:nobs],d4$ncl_2021.2030[1:nobs])

dpC2021.2030 <- depth(u0C,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv1C<-rev((1/dpC2021.2030) -1)
v1C<-scaleCurve_AD(u0C,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))


############################################

plot(v1$alpha,v1$Volume/1e+18, type='l', lwd=2,col='black' ,
     xlab="p", main="",
     xlim=c(0,1), ylim=c(0,ylim2),
     ylab = expression(Volume ~S[n](p)("1e+18")),panel.first = grid())


lines(v1C$alpha,v1C$Volume/1e+18, lty=2, lwd=2,cex = .8, col='blue')

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()

text(.8, 200,expression("1.70e+04"),cex = .8)



box()

legend("topleft", cex=1.0,title="Aggregate Loss",
       c('City A', 'City C'),
       lwd=c(2,2),
       lty=c(1,2),
       col=c('black', 'blue'))




#=====================================================


#############################################################################################
################################# Lorenz Curve ###########################
#par(mfrow=c(2,2))
ylim2=200

#--------------------2021.2030----------------------------------------


L1<-Lc(Outv1)
L3<-Lc(Outv1C)


plot(L1,col='black',lty=1, lwd=2,main="")
lines(L3,col='blue',lty=1, lwd=2)

grid()
legend("topleft", cex=1.0, title="Aggregate Loss",
       c('City A',  'City C'),
       lwd=c(2,2),
       lty=c(1,1),
       col=c('black', 'blue'))


polygon(c(L3$p, rev(L3$p)), c(L3$L, rev(L1$L)),
        col = "#6BD7AF")


lines(L1,col='black',lty=1, lwd=2)
lines(L3,col='blue',lty=1, lwd=2)

abline(0, 1,col='red',lty=1, lwd=2)


text(x=.4,y=0.5,"Line of equality",srt=32,col='black')
box()
