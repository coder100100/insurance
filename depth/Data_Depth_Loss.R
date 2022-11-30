
library(ddalpha)
library(DepthProc)
library(zoo)
library(ineq)


source('scaleCurve_AD.R')
source('ddPlot_AD.R')



##############################################City A Loss###################################################


d1<-read.csv("A_Loss_predicted_Clim1.csv")
d2<-read.csv("A_Loss_predicted_Clim2.csv")
d3<-read.csv("A_Loss_predicted_Clim3.csv")
d4<-read.csv("A_Loss_predicted_Clim4.csv")



#===============================================
#nobs=513
nobs=513  #


#===============================================




##################################################################################################
#------------------------2021.2030----------------------------------

u1<-cbind(d1$ncl_2021.2030[1:nobs],d2$ncl_2021.2030[1:nobs],d3$ncl_2021.2030[1:nobs],d4$ncl_2021.2030[1:nobs])


dpA <- depth(u1,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv1<-rev((1/dpA) -1)

Med_dp0G<-depthMedian(as.matrix(u1), depth_params = list(method = "Mahalanobis"))
Med_dp0G

dist_G<-mahalanobis(u1, Med_dp0G, cov(u1))
dist_G

v1<-scaleCurve_AD(u1,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))



###########################################################################################
#############################################################################################
#--------------------------City C-------------------------------
#--------------------------------------------------------------------------------------------

nobs=513


d1<-read.csv("C_Loss_predicted_Clim1.csv")
d2<-read.csv("C_Loss_predicted_Clim2.csv")
d3<-read.csv("C_Loss_predicted_Clim3.csv")
d4<-read.csv("C_Loss_predicted_Clim4.csv")




##################################################################################################
#------------------------2021.2030----------------------------------

u0C<-cbind(d1$X2021.2030[1:513],d2$X2021.2030[1:513],d3$X2021.2030[1:513],d4$X2021.2030[1:513],d5$X2021.2030[1:513],d6$X2021.2030[1:513])



dpC <- depth(u0C,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv0C<-rev((1/dpC) -1)

Med_dp0G<-depthMedian(as.matrix(u0C), depth_params = list(method = "Mahalanobis"))
Med_dp0G


dist_G<-mahalanobis(u0C, Med_dp0G, cov(u0C))
dist_G

v1C<-scaleCurve_AD(u0C,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))




##########################################################################################################
################ Scale curve ##############
#par(mfrow=c(2,2))
ylim2=2000

######################### cities A and C #############


#--------------------2021.2030----------------------------------------

plot(v1$alpha,v1$Volume/1e+17, type='l', lwd=2,col='black' , xlab="p", main="", 
     xlim=c(0,1), ylim=c(0,ylim2),ylab = expression(Volume ~S[n](p)("1e+17")),panel.first = grid())


lines(v1C$alpha,v1C$Volume/1e+17, lty=2, lwd=2,cex = .8, col='blue') 

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()

text(.8, 350,expression("1.32e+20"),cex = .8) 



box() 

legend("topleft", cex=1.0,
       c('City A', 'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))



#--------------------------2031.2040----------------------------------


#############################################################################################
################################# Lorenz Curve ###########################
#par(mfrow=c(2,2))
ylim2=200

#--------------------2021.2030----------------------------------------



L1<-Lc(Outv1)
L3<-Lc(Outv0C)



plot(L1,col='black',lty=1, lwd=2,main="")
lines(L3,col='blue',lty=2, lwd=2)

grid()
legend("topleft", cex=1.0,
       c('City A',  'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))


