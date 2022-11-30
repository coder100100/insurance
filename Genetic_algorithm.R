
library(GA)

#y0<-wsncl
#data22<-data.frame(y0,wspcp,wspcp_lag1, wmpcp1)

G_Algorithm<-function(data22,K)   # data22 include predicted variable as y0 # K-fold cross-validation
{
  

# Setup the data for cross-validation
# K = 5 # 5-fold cross-validation
fold_inds <- sample(1:K, nrow(data22), replace = TRUE)
lst_CV_data <- lapply(1:K, function(i) list(
  train_data = data22[fold_inds != i, , drop = FALSE], 
  test_data = data22[fold_inds == i, , drop = FALSE]))
 
#------------------------------------------------------------------------------------------------------
evalParams <- function(train_data, test_data, cost, gamma, epsilon) {
  model <- svm(y0 ~ ., data = train_data, cost = cost, gamma = gamma, epsilon = epsilon, type = "eps-regression", kernel = "radial")
  rmse <-sqrt(mean((predict(model, newdata = test_data) - test_data$y0) ^ 2))   # Test
  return (rmse)
}

fitnessFunc <- function(x, Lst_CV_Data) {
  cost_val <- x[1]   
  gamma_val <- x[2]
  epsilon_val <- x[3]
  rmse_vals <- sapply(Lst_CV_Data, function(in_data) with(in_data,evalParams(train_data, test_data, cost_val, gamma_val, epsilon_val)))
  return (-mean(rmse_vals))   # maximizing fitness implies minimizing the rmse
}

theta_min <- c(cost = 2, gamma = 2^-4, epsilon = 2^-4)  # Range of the parameter values to be tested
theta_max <- c(cost = 2^8, gamma = 2^2, epsilon = 2)


results<- ga(type = "real-valued", fitness = fitnessFunc, lst_CV_data,  names = names(theta_min), 
               min = theta_min, max = theta_max, popSize = 50, maxiter = 100)

#summary(results)
#plot(results) 
#cost--results@solution[1], gamma--results@solution[2], epsilon--results@solution[3]

#----------------------------------------------------------------------------------------------------------

param <- data.frame('cost'=results@solution[1], 'gamma'=results@solution[2], 'epsilon'=results@solution[3])
# param$cost

return(param)

}



# GA<-G_Algorithm(data22,5)
# GA$cost

