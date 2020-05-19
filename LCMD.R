###############################################################################
# Purpose: create functions for dignostics metrics for Loss Cost Models
# * WMSE, n-power
# * Psuedo R^2
# * Cov(y, y_hat)
# * Corr(y, y_hat)
# * Chi-Square
# * WMAD
# * Percentage Error
#
# References: NA
#
# Notes:
#  Added function to check if input vectors are the same length.
#  Changed variable names to conform with other functions.
#  Added rebalancing and policy aggregation to functions
###############################################################################

#---------------- wmse ----------------
wmse <- function(actual, pred_new, weight = rep(1, length(actual)), 
  policy_id=NULL, rbl = TRUE) {
 # Returns the weighted mean squared error as a scalar.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 # Optional Inputs:
 #   weight: a vector of weights set to a vector
 #          of 1's by default
 #   policy_id: a vector that contains policy numbers
 #             set by default to NULL
 #   rbl: TRUE if the predictions should be rebalanced
 #        to have the same average as the actual values
 #        FALSE if the predictions should not be rebalanced
 #
 # wmse = sum(weight*(pred_new - actual)^2)/sum(weight) 
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }
 
 ## Calculate the Weighted Mean Squared Error
 error_sq = (dd$pred_new - dd$actual)^2
 return(weighted.mean(error_sq, dd$weight))
}

#---------------- n-power ----------------
wmean_n_power_error <- function(actual, pred_new, n_power, 
  weight = rep(1, length(actual)), 
  policy_id=NULL, rbl = TRUE) {
 # Returns the weighted mean n-power error as a scalar.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 #  n_power: a numeric value
    
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #          of 1's by default
 #  policy_id: a vector that contains policy numbers
 #            set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #
 # wmean_n_power_error = sum(weight*(pred_new - actual)^n_power)/sum(weight) 
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }
 
 ## Calculate the Weighted Mean N-power Error
 error_sq = (dd$pred_new - dd$actual)^ n_power
 return(weighted.mean(error_sq, dd$weight))
}

#---------------- Pseudo R^2----------------
pseudo_r_sq <- function(actual, pred_new, 
  weight = rep(1, length(actual)), policy_id=NULL, rbl = TRUE ) {
 # Returns the pseudo R^2 as a scalar.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
    
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #       of 1's by default
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #
 # pseudo_r_sq = 1 - wmse/wmse_intercept
 # intercept = sum(weight * actual)/sum(weight)
 # wmse = sum(weight*(pred_new - actual)^2)/sum(weight)
 # wmse_intercept = sum(weight*(intercept - actual)^2)/sum(weight)
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }

 ## Calc WMSE Intercept
 intercept <- weighted.mean(dd$actual, dd$weight)
 error_intercept_sq <- (dd$actual - intercept)^2
 wmse_intercept <- weighted.mean(error_intercept_sq, dd$weight)
  
 ## Calc WMSE of the model
 error_sq = (dd$pred_new - dd$actual)^2
 wmse <- weighted.mean(error_sq, dd$weight)
  
 ## Return the pseudo_r_sq
 return(1 - wmse / wmse_intercept)
}


#-------- Cov (y, y_hat) ----------
model_cov <- function(actual, pred_new, 
  weight = rep(1, length(actual)), policy_id=NULL, rbl = TRUE){
 # Returns the weighted covariance between the 
 # actual vector and the pred_new vector as a scalar.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #       of 1's by default
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }
 

 ## Calc weighted covariance between model and actuals
 d <- data.frame(model = dd$pred_new, actual = dd$actual)
 c <- cov.wt(d, wt = dd$weight)
 ## cov.wt return a n x n covariance matrix
 ## diagonal entries are variances
 ## off-diagonal entries are covariance
 return (c$cov[1,2])

}


#------------ Corr (y, y_hat) ------------------
model_cor <- function(actual, pred_new, 
  weight = rep(1, length(actual)), policy_id=NULL, rbl = TRUE){
 # Returns the weighted correlation between the 
 # actual vector and the pred_new vector as a scalar.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
  
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #          of 1's by default
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }
 
 
 ## Calc weighted covariance between model and actuals
 d <- data.frame(model = dd$pred_new, actual = dd$actual)
 c <- cov.wt(d, wt = dd$weight) 
 ## cov.wt return a n x n covariance matrix
 ## diagonal entries are variances
 ## off-diagonal entries are covariance
 return(c$cov[1,2]/ (sqrt(c$cov[1,1])* sqrt(c$cov[2,2])))

}


#---------- chi squared ----------------
chi_square <- function(actual, pred_new, 
  weight = rep(1, length(actual)), policy_id=NULL, rbl = TRUE ) {
 # Returns the weighted chi-square.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #       of 1's by default
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #
 # error <- (actual - pred_new)^2 / pred_new
 # chi_square <- sum(weight * error)/sum(weight)
 
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }
 
 ## Calc weighted chi-squared
 error <- (dd$actual - dd$pred_new)^2 / dd$pred_new
 return(sum(dd$weight * error)/sum(dd$weight))
}

#------------- weighted percentage error -----------------
w_percentage_error <- function(actual, pred_new, 
  weight = rep(1, length(actual)), policy_id=NULL, rbl = TRUE ) {
 # Returns the weighted absolute value of percentage error.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
    
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #       of 1's by default
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #
 # error <- abs(actual/pred_new - 1)
 # w_percentage_error <- weighted.mean(error, weight)
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }
 
 index <- dd$pred_new > 0
  
 ## Calc weighted percentage error
 error <- abs(dd$actual[index]/dd$pred_new[index] - 1)
 
 return(weighted.mean(error, dd$weight[index]))
}

#----------- Weighte Mean Absolute Deviation ---------------
wmad <- function(actual, pred_new, 
  weight = rep(1, length(actual)), policy_id=NULL, rbl = TRUE ) {
 # Returns the weighted mean absolute deviation.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #       of 1's by default
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #
 # error <- abs(actual - pred_new)
 # wmad <- weighted.mean(error, weight)
 
 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual,pred_new, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl)
 {
  dd <- rebalance(actual,data.frame(pred_new), weight)
 }
 else
 {
  dd <- data.frame(actual,pred_new, weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)==0)
 {
  #print("Create item level diagnostic")
 }
 else
 {
  actual <- dd$actual
  pred_new <- dd$pred_new
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
 }
 
 ## Calculate weighted Mean Absolute Deviation
 error <- abs(dd$actual - dd$pred_new)
 return(weighted.mean(error, dd$weight))
}
