
library(lattice)
library(ggplot2)
library(e1071) 
library(zoo)

library(tidyverse)
library(tensorflow)statitic

library(keras)

#install_tensorflow()
#use_condaenv("r-tensorflow")

set.seed(2022)
#--------------------------------------------------------------------------------------------------------------------------------------


Data0<-read.csv('weeklydata.csv')
names(Data0) 

# "wsloss"     "wsncl"      "wspcp"      "wspcp_lag1" "wspcp_lag2" "wmpcp1"     "wmpcp_lag1"

##########################################################

######################################################################
wsloss_b<-Data0$wsloss/1000

m<-length(wsloss_b);                          # m 
wsloss0<-wsloss_b[2:m]                         # loss-Weakly sum; 


wsncl_b<-Data0$wsncl 
wsncl0<-wsncl_b[2:m]                     # ncl-Weakly sum


wspcp_b<-Data0$wspcp
wspcp0<-wspcp_b[2:m]                   # pcp-Weakly sum 
wspcp_lag10<-wspcp_b[1:m-1]                      # pcp-Weakly sum ; lag 1 

#wspcp_lag1<-Data0$wspcp_lag1
#wspcp_lag2<-Data0$wspcp_lag2

wmpcp_b<-Data0$wmpcp1                  #Weekly max
wmpcp0<-wmpcp_b[2:m]  
#wmpcp_lag1<-Data0$wmpcp_lag1





#################### ################################################################################

data_g<-data.frame(wsloss0,wspcp0,wspcp_lag10,wmpcp0,wsncl0)

datag20<-subset(data_g,data_g$wsncl0>0)
dim(datag20) #444      5

datag2<-datag20[1:440,]
dim(datag2)   # 440   5

wsloss<-datag2$wsloss0
wspcp<-datag2$wspcp0
wspcp_lag1<- datag2$wspcp_lag10
wsncl<-datag2$wsncl0
wmpcp<-datag2$wmpcp0

m22<-length(wsloss); m22 # 440


#----- year----
Years<-rep(2002:2011, each = 44)

length(Years)

#############################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------------------
N <- 1000 #                            # number of times to do the cross-validation
years <- c(2002:2011)
K <- c(5)   # 2, 3, 4, 5                    # years in a baseline/training period

#set.seed(123)                                # in case you will need resampling



RMSE_GASVR<-c()

RMSE_DNN<-c()
RMSE_glm<-c()




#-------------------------------------------------------------------------------------------------------------------------

for(k in 1:length(K)){ #k=1
  
  # All combinations of years, e.g., K[k] out of 10
  YearBaseline <- combn(years, K[k]) 
  # If there are not so many different combinations as many times you want to run the cross-validation (N),
  # add from resampling to have N:
  if(dim(YearBaseline)[2] < N) {
    tmp <- sample(c(1:dim(YearBaseline)[2]), (N-dim(YearBaseline)[2]), replace=TRUE)
    YearBaseline <- cbind(YearBaseline, YearBaseline[,tmp])
  }
  
  # If there are more combinations than N, need to truncate to N, here I do it by SRS of N columns 
  # (alternatively, can just take the first N columns of the YearBaseline matrix):
  YearBaseline <- YearBaseline[,sample(1:dim(YearBaseline)[2], N, replace=F)]
  # When the baseline years are selected (for fitting the models), combine them with different K[k] years for out-of-sample forecasting:
  YearProjected <- apply(YearBaseline, 2, function(x) sample(years[!is.element(years,x)],K[k], replace=F)) 
  
  
  #SL Squared errors (one error per i):
  #SE_IBA <- SE_QBA <- SE_GASVR <- SE_SVR <- SE_avNN <- SE_GLM<-rep(NA, N) #SL
  #--------------------------------------------------------------------------------------------------------------------------
  
  for(i in 1:N){ #i=1
    
    #--------------------------------------------------------------------------------------------------------------------------------
    #Baseline data 1
    wspcp_train <- wspcp[is.element(Years,YearBaseline[,i])]             #  there is no missing
    wspcp_lag1_train <- wspcp_lag1[is.element(Years,YearBaseline[,i])]
    wmpcp_train<-wmpcp[is.element(Years,YearBaseline[,i])]           
    
    wsncl_train <- wsncl[is.element(Years,YearBaseline[,i])]             #  there is no missing
    wsloss_train <- wsloss[is.element(Years,YearBaseline[,i])]
    
    
    
    #'Future' data 1
    wspcp_test <- wspcp[is.element(Years,YearProjected[,i])]            
    wspcp_lag1_test <- wspcp_lag1[is.element(Years,YearProjected[,i])]
    wmpcp_test<-wmpcp[is.element(Years,YearProjected[,i])]           
    wsncl_test <- wsncl[is.element(Years,YearProjected[,i])]             
    wsloss_test <- wsloss[is.element(Years,YearBaseline[,i])]
    
    mi <- min(c(length(wspcp_train), length(wspcp_test)))
    
    
    #------------------------ Training ---------------------------------------------
    #Modified Baseline data 
    wspcp_train<-wspcp_train[1:mi]; 
    wspcp_lag1_train<- wspcp_lag1_train[1:mi]; 
    wmpcp_train<-wmpcp_train[1:mi]
    wsncl_train<-wsncl_train[1:mi]
    wsloss_train<-wsloss_train[1:mi]
    
    
    y0<-wsloss_train
    x0<- data.frame(wspcp_train,wspcp_lag1_train,wmpcp_train,wsncl_train)  
    train_data<-data.frame(wsloss_train,wspcp_train,wspcp_lag1_train,wmpcp_train,wsncl_train)  

    #------------------------------------------ GA SVR--------------------------------------------------- 
   
    model_GASVR <- svm(x0,y0, cost =9.152059, gamma =0.1114998, epsilon = 0.2988295, type = "eps-regression", kernel = "radial")

    #---------- glm - Poisson Regression ------------------------------------------------
   
    model_glm<-glm(y0~wspcp_train+wspcp_lag1_train+wmpcp_train+wsncl_train, family = Gamma(link=log), maxit = 1000)
    
    #--------------------- DNN -------------------------------------------------------------------
    
    dataset<-data.frame(wsloss_train,wspcp_train,wspcp_lag1_train,wsncl_train,wsncl_train)
    
    
    lapply(dataset, function(x) sum(is.na(x))) %>% str()
    dataset <- na.omit(dataset)
    
    library(rsample)
    split <- initial_split(dataset, 0.8)
    train_dataset <- training(split)
    test_dataset <- testing(split)
    
    
    train_features <- train_dataset %>% select(-wsloss_train)
    test_features <- test_dataset %>% select(-wsloss_train)
    
    train_labels <- train_dataset %>% select(wsloss_train)
    test_labels <- test_dataset %>% select(wsloss_train)
    
    
    normalizer <- layer_normalization(axis = -1L)
    normalizer %>% adapt(as.matrix(train_features))
    print(normalizer$mean)
    
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
    
    
    dnn_model <- build_and_compile_model(normalizer)
    summary(dnn_model)
    
    
    # Use Keras Model.fit to execute the training for 10 epochs:
    history <- dnn_model %>% fit(
      as.matrix(train_features),
      as.matrix(train_labels),
      validation_split = 0.2,
      verbose = 0,
      epochs = 30
    )
    
    ### Loss 
    history
    plot(history)
    
    
    
    
   ##################################################################################
   #---------------------------Testing ---------------------------------------------
    #'Future' data 2
    wspcp_test<-wspcp_test[1:mi]; 
    wspcp_lag1_test<- wspcp_lag1_test[1:mi]; 
    wmpcp_test<-wmpcp_test[1:mi]
    wsncl_test<-wsncl_test[1:mi]
    wsloss_test<-wsloss_test[1:mi] 
    x1<- data.frame(wspcp_test,wspcp_lag1_test,wmpcp_test,wsncl_test) 
    
    
    #--------------GASVR-------------------------------------------------------
    
    a1<-predict(model_GASVR, x1, type="response")
    am1<-mean(na.omit(a1))
    aa1<-a1 %>% replace_na(am1)  # Replacing NA with the mean
    
    
    RMSE_GASVR2<- sqrt(mean((wsloss_test- aa1)^2))  #}   # GASVR  # Prediction and RMSE
    RMSE_GASVR<-c(RMSE_GASVR,RMSE_GASVR2)
    
  #------------------------ GLM-------------------------------------------------
 
    a3<-predict(model_glm, x1, type="response")
    am3<-mean(na.omit(a3))
    aa3<-a3%>% replace_na(am3)  # Replacing NA with the mean
    
    RMSE_glm2<- sqrt(mean((wsloss_test- aa3)^2))            # GLM
    RMSE_glm<-c(RMSE_glm,RMSE_glm2)
    
    #--------------------------- DNN ---------------------------------------------
  
    a2<-predict(dnn_model, as.matrix(x1))
    am2<-mean(na.omit(a2))
    aa2<-a2 %>% replace_na(am2)  # Replacing NA with the mean
    
    
    RMSE_DNN2<- sqrt(mean((wsloss_test- aa2)^2))  #}   # GASVR  # Prediction and RMSE
    RMSE_DNN<-c(RMSE_DNN,RMSE_DNN2)
    
 
  
    
  }
  

  
}

 
mat_RMSE0<-data.frame(RMSE_glm,RMSE_DNN,RMSE_GASVR)

#mat_RMSE<-as.matrix(na.omit(mat_RMSE0))  
#dim(mat_RMSE)
 

########################################################################
 
m_RMSE_glm<-mean(mat_RMSE0$RMSE_glm)
m_RMSE_DNN<-mean(mat_RMSE0$RMSE_DNN)
m_RMSE_GASVR<-mean(mat_RMSE0$RMSE_GASVR)

aveR<-c(m_RMSE_glm,m_RMSE_DNN,m_RMSE_GASVR)
aveR

Methods<-c("GLM", "DNN","GASVR")
data.frame(Methods, 'Period 5 years'=aveR)


##################################################################


NC<-(5*sum(wsloss0))/10
N_RMSE_glm<-(mean(mat_RMSE0$RMSE_glm)/NC)*100
N_RMSE_DNN<-(mean(mat_RMSE0$RMSE_DNN)/NC)*100
N_RMSE_GASVR<-(mean(mat_RMSE0$RMSE_GASVR)/NC)*100

N_RMSE<-c(N_RMSE_glm,N_RMSE_DNN,N_RMSE_GASVR)
data.frame(Methods, 'Period 5 years'=N_RMSE)

















