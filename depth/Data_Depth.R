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

u1B<-cbind(d1$X2031.2040,d2$X2031.2040[1:513])

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


##########################################################################
#--------------------------City B--------------------------------

d1<-read.csv("B_NCL_predicted_Clim1.csv")
d2<-read.csv("B_NCL_predicted_Clim2.csv")
d3<-read.csv("B_NCL_predicted_Clim3.csv")
d4<-read.csv("B_NCL_predicted_Clim4.csv")

d5<-read.csv("B_NCL_predicted_Clim5.csv")
d6<-read.csv("B_NCL_predicted_Clim6.csv")


#------------------------2021.2030----------------------------------
u0B<-cbind(d1$X2021.2030,d2$X2021.2030[1:513],d3$X2021.2030[1:513],d4$X2021.2030[1:513],d5$X2021.2030[1:513],d6$X2021.2030[1:513])


dpB <- depth(u0B,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv0B<-rev((1/dpB) -1)

Med_dp0G<-depthMedian(as.matrix(u0B), depth_params = list(method = "Mahalanobis"))
Med_dp0G



#cov(zG)
dist_G<-mahalanobis(u0B, Med_dp0G, cov(u0B))
dist_G

v1B<-scaleCurve_AD(u0B,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))


##########################################################################
#--------------------------City C-------------------------------
#--------------------------------------

d1<-read.csv("C_NCL_predicted_Clim1.csv")
d2<-read.csv("C_NCL_predicted_Clim2.csv")
d3<-read.csv("C_NCL_predicted_Clim3.csv")
d4<-read.csv("C_NCL_predicted_Clim4.csv")

d5<-read.csv("C_NCL_predicted_Clim5.csv")
d6<-read.csv("C_NCL_predicted_Clim6.csv")

u0C<-cbind(d1$X2021.2030,d2$X2021.2030[1:513],d3$X2021.2030[1:513],d4$X2021.2030[1:513],d5$X2021.2030[1:513],d6$X2021.2030[1:513])



dpC <- depth(u0C,method = "Mahalanobis")  #"Mahalanobis", "Euclidean" or "Tukey"
Outv0C<-rev((1/dpC) -1)

Med_dp0G<-depthMedian(as.matrix(u0C), depth_params = list(method = "Mahalanobis"))
Med_dp0G




#================== two climate model ================

u0CB<-cbind(d1$X2021.2030,d2$X2021.2030[1:513])


depthContour(u0CB, depth_params = list(method = "Mahalanobis"),xlim=c(-3,15),ylim=c(-3,16), points = TRUE, 
             legend = FALSE,levels=10, mcol = "blue")




#============== Bagplot=====================================

Result<-compBagplot(u0CB, type = "projdepth", sizesubset = 500,
                    extra.directions = FALSE, options = NULL)
bagplot(Result, plot.fence = TRUE)





####### Med #########
# A: 2.839475 2.765683 2.221376 3.144018 2.421353 2.675358
# B: 2.529397 2.293748 3.093170 2.755355 2.689556 2.223498
# C: 3.193559 3.802572 3.212238 3.532729 2.857821 2.744233


#cov(zG)
dist_G<-mahalanobis(u0C, Med_dp0G, cov(u0C))
dist_G

v1C<-scaleCurve_AD(u0C,alpha=seq(0,1,0.1), depth_params = list(method = "Mahalanobis"))




##########################################################################################################
################ Scale curve ##############
#par(mfrow=c(1,2))

plot(v1$alpha,v1$Volume/1e+6, type='l', lty=5,lwd=2,col='blue' , xlab="p", main="", ylim=c(0,250),ylab = expression(Volume ~S[n](p)))

lines(v2$alpha,v2$Volume/1e+6,type='o',pch = 1, lwd=2,cex = .7,  col='red')

lines(v3$alpha,v3$Volume/1e+6, type='o',pch = 0,lwd=2,cex = .6, col='green') 

lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
grid()

legend("topleft", cex=1.0,
       c( '2021-2030','2031-2040', '2041-2050','2051-2060'),
       lty=c(2,1,1,1),  pch = c(NA,1,0,6), lwd=c(2,2,2,2),
       col=c('blue','red','green','black'))



box() 


####################################################################################
######################### 3 cities #############
plot(v1$alpha,v1$Volume/1e+3, type='l', lwd=2,col='black' , xlab="p", main="", ylim=c(0,350),ylab = expression(Volume ~S[n](p)))

lines(v1B$alpha,v1B$Volume/1e+3,type='o',pch = 6, lwd=2,cex = .8,  col='green')

lines(v1C$alpha,v1C$Volume/1e+3, type='o',pch = 0,lwd=2,cex = .8, col='blue') 

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()


box() 


legend("topleft", cex=1.0,
       c('City A', 'City B', 'City C'),
       lwd=c(2,2,2), 
       lty=c(1,1,1),
       pch = c(NA,6,0),
       col=c('black', 'green','blue'))




######################### cities A and C #############
plot(v1$alpha,v1$Volume/1e+3, type='l', lwd=2,col='black' , xlab="p", main="", 
     xlim=c(0,1), ylim=c(0,350),ylab = expression(Volume ~S[n](p)("1e+3")),panel.first = grid())

#lines(v1B$alpha,v1B$Volume/1e+3,type='o',pch = 6, lwd=2,cex = .8,  col='green')

lines(v1C$alpha,v1C$Volume/1e+3, lty=2, lwd=2,cex = .8, col='blue') 

#lines(v4$alpha,v4$Volume/1e+6, type='o', pch = 6,lwd=2,cex = .6, col='black')
#grid()

text(.8, 350,expression("1.32e+05"),cex = .8) 



box() 

legend("topleft", cex=1.0,
       c('City A', 'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))



#################################

L1<-Lc(Outv1)
L2<-Lc(Outv0B)
L3<-Lc(Outv0C)



plot(L1,col='black',lty=1, lwd=2,main="")
#lines(L2,col='green',lty=5, lwd=2)
lines(L3,col='blue',lty=2, lwd=2)

grid()
legend("topleft", cex=1.0,
       c('City A',  'City C'),
       lwd=c(2,2), 
       lty=c(1,2),
       col=c('black', 'blue'))

###################################################################

L1<-Lc(dpA)
L2<-Lc(dpB)
L3<-Lc(dpC)



plot(L1,col='black',lty=1, lwd=2,main="")
lines(L2,col='green',lty=5, lwd=2)
lines(L3,col='blue',lty=6, lwd=2)


legend("topleft", cex=1.0,
       c('City A', 'City B', 'City C'),
       lwd=c(2,2,2), 
       lty=c(1,5,6),
       col=c('black', 'green','blue'))




