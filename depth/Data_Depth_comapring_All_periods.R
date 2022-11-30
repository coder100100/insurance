library(ddalpha)
library(DepthProc)
library(zoo)
library(ineq)




source('scaleCurve_AD.R')
source('ddPlot_AD.R')

##############################################City A NCL###################################################


d1<-read.csv("A_NCL_predicted_Clim1.csv")
d2<-read.csv("A_NCL_predicted_Clim2.csv")
d3<-read.csv("A_NCL_predicted_Clim3.csv")
d4<-read.csv("A_NCL_predicted_Clim4.csv")

d5<-read.csv("A_NCL_predicted_Clim5.csv") 
d6<-read.csv("A_NCL_predicted_Clim6.csv")

##################################################################################################
#------------------------2021.2030----------------------------------

u1<-cbind(d1$X2021.2030,d2$X2021.2030[1:513],d3$X2021.2030[1:513],d4$X2021.2030[1:513],d5$ncl_2031.2040[1:513],d6$ncl_2031.2040[1:513])


dpA <- depth(u1,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv1<-rev((1/dpA) -1)

Med_dp0G<-depthMedian(as.matrix(u1), depth_params = list(method = "Mahalanobis"))
Med_dp0G
# 4.021547 3.460985 3.882129 3.123517 3.841504 3.834690
#cov(zG)
dist_G<-mahalanobis(u1, Med_dp0G, cov(u1))
dist_G

v1<-scaleCurve_AD(u1,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))



#================== two climate model ================

u1B<-cbind(d1$X2021.2030,d2$X2021.2030[1:513])

depthContour(u1B, depth_params = list(method = "Mahalanobis"),xlim=c(-3,15),ylim=c(-3,16), points = TRUE, 
             legend = FALSE,levels=10, mcol = "blue")



#============== Bagplot=====================================
library(mrfDepth) # for bagplot


Result<-compBagplot(u1B, type = "projdepth", sizesubset = 500,
                    extra.directions = FALSE, options = NULL)
bagplot(Result, plot.fence = TRUE)


#############################################################################
#--------------------------2031.2040----------------------------------


u2<-cbind(d1$X2031.2040,d2$X2031.2040[1:513],d3$X2031.2040[1:513],d4$X2031.2040[1:513],d5$ncl_2031.2040[1:513],d6$ncl_2031.2040[1:513])

dp0G <- depth(u2,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv2<-rev((1/dp0G) -1)

Med_dp0G<-depthMedian(as.matrix(u2), depth_params = list(method = "Mahalanobis"))
Med_dp0G
#cov(zG)
dist_G<-mahalanobis(u2, Med_dp0G, cov(u2))
dist_G

v2<-scaleCurve_AD(u2,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))



#================== two climate model ================

u1B<-cbind(d1$X2031.2040[1:513],d2$X2031.2040[1:513])

depthContour(u1B, depth_params = list(method = "Mahalanobis"),xlim=c(-3,15),ylim=c(-3,16), points = TRUE, 
             legend = FALSE,levels=10, mcol = "blue")




#################################################
#--------------------------2041.2050----------------------------------


u3<-cbind(d1$X2041.2050,d2$X2041.2050[1:513],d3$X2041.2050[1:513],d4$X2041.2050[1:513],d5$ncl_2041.2050[1:513],d6$ncl_2041.2050[1:513])

dp0G <- depth(u3,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv3<-rev((1/dp0G) -1)

Med_dp0G<-depthMedian(as.matrix(u3), depth_params = list(method = "Mahalanobis"))
Med_dp0G

# 2.839475 2.765683 2.221376 3.144018 2.421353 2.675358


#cov(zG)
dist_G<-mahalanobis(u3, Med_dp0G, cov(u3))
dist_G

v3<-scaleCurve_AD(u3,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))

##########################################################################
#--------------------------2051.2060----------------------------------

u4<-cbind(d1$X2051.2060,d2$X2051.2060[1:513],d3$X2051.2060[1:513],d4$X2051.2060[1:513],d5$ncl_2051.2060[1:513],d6$ncl_2051.2060[1:513])

dp0G <- depth(u4,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv4<-rev((1/dp0G) -1)

Med_dp0G<-depthMedian(as.matrix(u4), depth_params = list(method = "Mahalanobis"))
Med_dp0G
#cov(zG)
dist_G<-mahalanobis(u3, Med_dp0G, cov(u4))
dist_G

v4<-scaleCurve_AD(u4,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))




###########################################################################################
#############################################################################################
#--------------------------City C-------------------------------
#--------------------------------------------------------------------------------------------

nobs=513

d1<-read.csv("C_NCL_predicted_Clim1.csv")
d2<-read.csv("C_NCL_predicted_Clim2.csv")
d3<-read.csv("C_NCL_predicted_Clim3.csv")
d4<-read.csv("C_NCL_predicted_Clim4.csv")

d5<-read.csv("C_NCL_predicted_Clim5.csv")
d6<-read.csv("C_NCL_predicted_Clim6.csv")


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



#############################################################################
#--------------------------2031.2040----------------------------------


u2c<-cbind(d1$X2031.2040[1:nobs],d2$X2031.2040[1:nobs],d3$X2031.2040[1:nobs],d4$X2031.2040[1:nobs],d5$ncl_2031.2040[1:nobs],d6$ncl_2031.2040[1:nobs])

dpC2031.2040 <- depth(u2c,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv2C<-rev((1/dpC2031.2040) -1)

v2C<-scaleCurve_AD(u2c,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))

#################################################
#--------------------------2041.2050----------------------------------


u3c<-cbind(d1$X2041.2050[1:nobs],d2$X2041.2050[1:nobs],d3$X2041.2050[1:nobs],d4$X2041.2050[1:nobs],d5$ncl_2041.2050[1:nobs],d6$ncl_2041.2050[1:nobs])

dpC2041.2050 <- depth(u3c,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv3C<-rev((1/dpC2041.2050) -1)



v3C<-scaleCurve_AD(u3c,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))

##########################################################################
#--------------------------2051.2060----------------------------------

u4c<-cbind(d1$X2051.2060[1:nobs],d2$X2051.2060[1:nobs],d3$X2051.2060[1:nobs],d4$X2051.2060[1:nobs],d5$ncl_2051.2060[1:nobs],d6$ncl_2051.2060[1:nobs])

dpC2051.2060 <- depth(u4c,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv4C<-rev((1/dpC2051.2060) -1)


v4C<-scaleCurve_AD(u4c,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))





##########################################################################################################
################ Scale curve ##############
#par(mfrow=c(2,2))
ylim2=200

######################### cities A and C #############


#--------------------2021.2030----------------------------------------

plot(v1$alpha,v1$Volume/1e+3, type='l', lwd=2,col='black' , xlab="p", main="", 
     xlim=c(0,1), ylim=c(0,ylim2),ylab = expression(Volume ~S[n](p)("1e+3")),panel.first = grid())


lines(v1C$alpha,v1C$Volume/1e+3, lty=2, lwd=2,cex = .8, col='blue') 

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()

text(.8, 300,expression("1.32e+05"),cex = .8) 



box() 

legend("topleft", cex=1.0, title="Number of Claims",
       c('City A', 'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))



#--------------------------2031.2040----------------------------------


plot(v2$alpha,v2$Volume/1e+3, type='l', lwd=2,col='black' , xlab="p", main="", 
     xlim=c(0,1), ylim=c(0,ylim2),ylab = expression(Volume ~S[n](p)("1e+3")),panel.first = grid())


lines(v2C$alpha,v2C$Volume/1e+3, lty=2, lwd=2,cex = .8, col='blue') 

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()

text(.8, 350,expression("1.32e+05"),cex = .8) 



box() 

legend("topleft", cex=1.0,
       c('City A', 'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))




#--------------------------2041.2050----------------------------------


plot(v3$alpha,v3$Volume/1e+3, type='l', lwd=2,col='black' , xlab="p", main="", 
     xlim=c(0,1), ylim=c(0,ylim2),ylab = expression(Volume ~S[n](p)("1e+3")),panel.first = grid())


lines(v3C$alpha,v3C$Volume/1e+3, lty=2, lwd=2,cex = .8, col='blue') 

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()

text(.8, 350,expression("1.32e+05"),cex = .8) 



box() 

legend("topleft", cex=1.0,
       c('City A', 'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))


#--------------------------2051.2060----------------------------------


plot(v4$alpha,v4$Volume/1e+3, type='l', lwd=2,col='black' , xlab="p", main="", 
     xlim=c(0,1), ylim=c(0,ylim2),ylab = expression(Volume ~S[n](p)("1e+3")),panel.first = grid())


lines(v4C$alpha,v4C$Volume/1e+3, lty=2, lwd=2,cex = .8, col='blue') 

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()

text(.8, 350,expression("1.32e+05"),cex = .8) 



box() 

legend("topleft", cex=1.0,
       c('City A', 'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))




######################### City A all period #############

#par(mfrow=c(1,1))

ylim2=300

#--------------------2021.2030----------------------------------------

plot(v1$alpha,v1$Volume/1e+3, type='l', lwd=2,col='black' , xlab="p", main="", 
     xlim=c(0,1), ylim=c(0,ylim2),ylab = expression(Volume ~S[n](p)("1e+3")),panel.first = grid())


lines(v2$alpha,v2$Volume/1e+3, lty=2, lwd=2,cex = .8, col='blue') 

lines(v3$alpha,v3$Volume/1e+3, lty=2, lwd=2,cex = .8, col='red') 
lines(v4$alpha,v4$Volume/1e+3, lty=2, lwd=2,cex = .8, col='green') 


#grid()

text(.8, 350,expression("1.32e+05"),cex = .8) 



box() 

legend("topleft", cex=1.0,
       c('2021.2030', '2031.2040','2041.2050','2051.2060'),
       lwd=c(2,2,2,2), 
       lty=c(1,2,2,2),
       col=c('black', 'blue','red', 'green'))


#############################################################################################
################################# Lorenz Curve ###########################
#par(mfrow=c(2,2))
ylim2=200

#--------------------2021.2030----------------------------------------


L1<-Lc(Outv1)
L3<-Lc(Outv0C)


plot(L1,col='black',lty=1, lwd=2,main="")
lines(L3,col='blue',lty=1, lwd=2)

grid()
legend("topleft", cex=1.0, title="Number of Claims",
       c('City A',  'City C'),
       lwd=c(2,2), 
       lty=c(1,1),
       col=c('black', 'blue'))


polygon(c(L3$p, rev(L3$p)), c(L3$L, rev(L1$L)),
        col = "#6BD7AF")


lines(L1,col='black',lty=1, lwd=2)
lines(L3,col='blue',lty=1, lwd=2)

abline(0, 1,col='red',lty=1, lwd=2)

box()
text(x=.4,y=0.5,"Line of equality",srt=30,col='black')

#--------------------2031.2040----------------------------------------

L1<-Lc(Outv2)
L3<-Lc(Outv2C)


plot(L1,col='black',lty=1, lwd=2,main="")
lines(L3,col='blue',lty=2, lwd=2)

grid()
legend("topleft", cex=1.0,
       c('City A',  'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))









#--------------------2041.2050----------------------------------------

L1<-Lc(Outv3)
L3<-Lc(Outv3C)


plot(L1,col='black',lty=1, lwd=2,main="")
lines(L3,col='blue',lty=2, lwd=2)

grid()
legend("topleft", cex=1.0,
       c('City A',  'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))



#--------------------2051.2060----------------------------------------

L1<-Lc(Outv4)
L3<-Lc(Outv4C)


plot(L1,col='black',lty=1, lwd=2,main="")
lines(L3,col='blue',lty=2, lwd=2)

grid()
legend("topleft", cex=1.0,
       c('City A',  'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))






