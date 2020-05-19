
###############################################################################
# Purpose: create functions for dignostics metrics
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

## wmse
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

## n-power
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

## Pseudo R^2
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


## Cov (y, y_hat)
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


## Corr (y, y_hat)
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



## chi squared
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

## weighted percentage error
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

## Weighte Mean Absolute Deviation
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
 
 
 ## Calc weighted Mean Absolute Deviation
 error <- abs(dd$actual - dd$pred_new)
 return(weighted.mean(error, dd$weight))
}

 
 
#####################
# Program Name:   AIC_BIC.R 
# Purpose: Generate Akaike Information Criterion (AIC) and 
#          Bayesian Information Criterion (BIC) statistics
#
# Data Description: Simulated Tweedie data
#
# References:  N/A
#
# Usage: See function inputs and outputs 
#
#####################

# input is 3 vectors of equal length, and p (number of model parameters)
    # actual is the observed loss costs
    # pred_new is the proposed loss cost predictions 
    # weight is exposures, default to 1
    # p is the number of parameters (including intercept)
    
    # Optional Inputs:
    #    policy_id: a vector that contains policy numbers
    #                set by default to NULL
    #    rbl: TRUE if the predictions should be rebalanced
    #              to have the same average as the actual values
    #         FALSE if the predictions should not be rebalanced

# output is AIC and BIC


AIC_BIC <- function(actual,pred_new,weight=rep(1,length(actual)), p, policy_id=NULL,rbl=TRUE) { 
    # Returns a list of two scalars (AIC and BIC)
    # 
    # Inputs: 
    #  actual: a vector of actual values
    #  pred_new: a vector of predicted values based on proposed model 
    #  weight: a vector of weights, default to 1
    #  p: the number of model parameters (including intercept)
    #  Optional Inputs:
    #    policy_id: a vector that contains policy numbers
    #                set by default to NULL
    #    rbl: TRUE if the predictions should be rebalanced
    #         to have the same average as the actual values
    #        FALSE if the predictions should not be rebalanced

        ## run data prep code
        # check that vectors are the same length
        if(length(policy_id)==0){
            if(!is_same_length(list(actual,pred_new,weight))){
                return("Error: Inputs are not the same length")
            }
        }
        else{
            if(!is_same_length(list(actual,pred_new,weight,policy_id))){
                return("Error: Inputs are not the same length")
            }
        }
        
        # rebalance predictions to match actuals if rbl is TRUE
        if(rbl){
            dd <- rebalance(actual,data.frame(pred_new),weight)
        }
        else{
            dd <- data.frame(actual,pred_new,weight)
        }

        #summarize to policy level if that option is specified
        if(length(policy_id)!=0){
            actual <- dd$actual
            pred_new <- dd$pred_new
            weight <- dd$weight
            dd <- policy_level(data.frame(actual,pred_new),weight,policy_id)
        }


    resid2 = (weight * (dd$pred_new - dd$actual))^2

    rss0 = sum(resid2)

    n <- nrow(dd)

    AIC_value = n + n * log(2 * pi) + n * log(rss0 / n) + 2 * (p + 1)

    BIC_value = n + n * log(2 * pi) + n * log(rss0 / n) + log(n) * (p + 1)

    z <- c(list(AIC = AIC_value,
                BIC = BIC_value
           ))                                                                                                                                                                                                                                                           
    z                                                                                                                                      
}
   
#########################################
# Program Name:   Lorenz_Gini.R 
# Purpose: Create Lorenz Curve and Gini Index, and return a list
#           of 2 vectors and 1 Gini Index
#
# Data Description: Simulated Tweedie data
#
# References:  
#
# Usage: See function inputs and outputs 
#
#########################################  

# input is 4 vectors of equal length, a method option ('expsoure' or 'premium') and a boolean for plot control   
    # actual is the actualloss cost 
    # pred_new1 is the proposed loss cost predictions 
    # pred_current is the current loss cost predictions 
    # weight is exposures, default to 1
    # method is an option to choose premium-based or exposure-based Lorenz Curve and Gini Index
    #         (default is exposure-based method)
    # plot is TRUE or FALSE
    
    # Optional Inputs:
    #    policy_id: a vector that contains policy numbers
    #                set by default to NULL
    #    rbl: TRUE if the predictions should be rebalanced
    #         to have the same average as the actual values
    #         FALSE if the predictions should not be rebalanced

# output is a Lorenz Curve plot, two vectors and a Gini Index

Lorenz_Gini <- function(actual, pred_new, pred_current=NULL, weight=rep(1,length(actual)), method="exposure", policy_id=NULL,rbl=TRUE, plot=TRUE) {    
    # Returns one scalar (Gini Index) and two vectors to plot Lorenz curve
    # 
    # Inputs: 
    #  actual: a vector of actual values
    #  pred_new: a vector of predicted values based on proposed model 
    #  pred_current: a vector of predicted values based on current model
    #  weight: a vector of weights 
    #  method is an option to choose premium-based or exposure-based Lorenz Curve and Gini Index (default is 'exposure')
    
    #  Optional Inputs:
    #    policy_id: a vector that contains policy numbers
    #                set by default to NULL
    #    rbl: TRUE if the predictions should be rebalanced
    #         to have the same average as the actual values
    #         FALSE if the predictions should not be rebalanced


       if ((tolower(method) != 'exposure') && (tolower(method) != 'premium')) {
           stop("Error: The method parameter has to be either 'Exposure' or 'Premium'!")
       }
       if ((tolower(method) == 'exposure') && (length(pred_current) > 0)) {
           cat("Note: The 'exposure' based method only requires two vectors of loss values. The third vector was not used.\n")
       }

       else if ((tolower(method) == 'exposure') && (length(pred_current) == 0)) {
           pred_current = rep(weighted.mean(actual,weight),length(actual))
       }
       else if ((tolower(method) == 'premium') && (length(pred_current) == 0)) {
           stop("Error: There are only two vectors of loss values as input. The 'premium' based method requires three vectors of loss values.")
       }

        ## run data prep code
        # gather vectors
        if (length(pred_current)==0){
            if(length(policy_id)==0){
                dlist <- list(actual,pred_new,weight)
                predframe <- data.frame(pred_new)
                dframe <- data.frame(actual,pred_new)
            }else{
                dlist <- list(actual,pred_new,policy_id,weight)
                predframe <- data.frame(pred_new)
                dframe <- data.frame(actual,pred_new)               
            }
        }else{
            if(length(policy_id)==0){
                dlist <- list(actual,pred_new,pred_current,weight)
                predframe <- data.frame(pred_new,pred_current)
                dframe <- data.frame(actual,pred_new,pred_current)
            }else{
                dlist <- list(actual,pred_new,policy_id,weight)
                predframe <- data.frame(pred_new,pred_current)
                dframe <- data.frame(actual,pred_new,pred_current)               
            }
        }
        
        # check that vectors are the same length
        if(!is_same_length(dlist)){
            return("Error: Inputs are not the same length")
        }
        
        # rebalance predictions to match actuals if rbl is TRUE
        if(rbl){
            dd <- rebalance(actual,predframe,weight)
        }else{
            dd <- data.frame(actual,predframe,weight)
        }

        #summarize to policy level if that option is specified
        if(length(policy_id)!=0){
            dd <- policy_level(dd[,-length(dd)],weight,policy_id)
        }

   o <- order(dd$pred_new)
   dd <- dd[o,]
   n <- nrow(dd)+1
   cumulative_actual <- c(0, cumsum(dd$actual * dd$weight) / sum(dd$actual * dd$weight)) 

   if (tolower(method) == 'exposure') {

       cumulative_exposure <- c(0, cumsum(dd$weight) / sum(dd$weight)) 
       Gini_index_expos=sum(cumulative_actual[-1] * cumulative_exposure[-n]) - sum(cumulative_actual[-n] * cumulative_exposure[-1]) 
 
   
       if(plot==TRUE)
       {

           windows()
           plot(cumulative_exposure, cumulative_actual,
                xlab= '% Exposure',
                ylab= '% Actual Loss', 
                main = paste('Gini Index =', format(100*Gini_index_expos, digit=2), "%"),
                col=2,
                ty='l'
                )
           abline(0,1) 
    
          legend.txt <- c('Random','Lorenz Curve')
          legend("topleft", legend= legend.txt, 
               col = c(1,2),
               lty = c(1,1),
               lwd = c(1,2),
               bty = "o", cex = 0.8)
       }

      z <- c(list(Gini_index  = Gini_index_expos))
      z$cumulative_actual <- cumulative_actual
      z$cumulative_exposure <- cumulative_exposure 
   }

   if (tolower(method) == 'premium') {
  
       cumulative_premium <- c(0, cumsum(dd$pred_current * dd$weight) / sum(dd$pred_current * dd$weight))
       Gini_index_prem=sum(cumulative_actual[-1] * cumulative_premium[-n]) - sum(cumulative_actual[-n] * cumulative_premium[-1]) 
 
       if(plot==TRUE)
       { 
           windows()
           plot(cumulative_premium, cumulative_actual,
                xlab= '% Premium',
                ylab= '% Actual Loss', 
                main = paste('Gini Index =', format(100*Gini_index_prem, digit=2), "%"),
                col=2,
                ty='l'
                )
           abline(0,1) 
 
          legend.txt <- c('Random','Lorenz Curve')
          legend("topleft", legend= legend.txt, 
              col = c(1,2),
              lty = c(1,1),
              lwd = c(1,2),
              bty = "o", cex = 0.8)
      }
      z <- c(list(Gini_index = Gini_index_prem))
      z$cumulative_actual <- cumulative_actual
      z$cumulative_premium <- cumulative_premium

  }
 z
}



######################################
# Program Name: meyers.R 
# 
# Purpose: Recreate diagnostics from Glenn Meyer's presentations
#
# Data Description: Simulated Tweedie data
#
# References: http://www.casact.org/education/annual/2007/handouts/meyers.ppt
#
# Usage: See function inputs and outputs
# selective_underwriting()
# adverse_selection() 
# value_of_lift()
#
# Requirements: lattice
#
######################################
#require(lattice)

selective_underwriting <- function(actual,pred_new,pred_current,weight=rep(1,length(actual)),xvalues=c(.75,.80,.85,.90,.95),policy_id=NULL,rbl=TRUE,plot=TRUE)
    {
     # Returns values to create Selective Underwriting plot invented by Glenn Meyers
        #     x :  plot values for the x-axis
        #     lr_decrease: The decrease in loss ratio caused by selecting lower relativities
        #    
        # Plots "The Effect of Selecting Lower Relativite" graph
        #
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 #  pred_current: a vector of baseline predictions
       
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #          of 1's by default
 #  xvalues: A vector of the percent of items selected.  Plotted on the x-axis
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #  plot: TRUE if a plot should be produced
 #        FALSE if the plot should be surpressed

        ## run data prep code
        # check that vectors are the same length
        if(length(policy_id)==0){
            if(!is_same_length(list(actual,pred_new,pred_current,weight))){
                return("Error: Inputs are not the same length")
            }
        }
        else{
            if(!is_same_length(list(actual,pred_new,pred_current,weight,policy_id))){
                return("Error: Inputs are not the same length")
            }
        }
        
        # rebalance predictions to match actuals if rbl is TRUE
        if(rbl){
            dd <- rebalance(actual,data.frame(pred_new,pred_current),weight)
        }
        else{
            dd <- data.frame(actual,pred_new,pred_current,weight)
        }

        #summarize to policy level if that option is specified
        if(length(policy_id)!=0){
            actual <- dd$actual
            pred_new <- dd$pred_new
            pred_current <- dd$pred_current
            weight <- dd$weight
            dd <- policy_level(data.frame(actual,pred_new,pred_current),weight,policy_id)
        }

    # create meyers vars
    dd$relativity <- dd$pred_new / dd$pred_current
    dd$wa <- dd$actual*dd$weight 
    dd$wc <- dd$pred_current*dd$weight
    dd_sorted_asc <- dd[order(dd$relativity),]
    l <- length(dd$relativity)
    avg_lr <- sum(dd$wa) / sum(dd$wc)
    rm(dd)
    
    # create "Effect of Selecting Lower Relativities" graph
    # sort the xvalues in increaseing order
    x <- xvalues[order(xvalues)]
    # calculate the loss ratio decrease for each xvalue
    lr_decrease <- vector(length=length(x))
    for (i in 1:length(x)){
        lr_decrease[i] <- 1 - (sum(dd_sorted_asc[1:I(x[i]*l),"wa"]) / sum(dd_sorted_asc[1:I(x[i]*l), "wc"])) / avg_lr        
    }

    if(plot==TRUE)
        {
        windows()
        barplot(lr_decrease*100
            ,xlab="% Items Selected"
            ,ylab="% Decrease in Loss Ratio"
            ,names.arg=xvalues*100
            ,main="Effect of Selecting Lower Relativites"
            )
        }
    plot_values <- data.frame(x,lr_decrease)
    return(plot_values)
    }

#adverse selection
adverse_selection <- function(actual,pred_new,pred_current,weight=rep(1,length(actual)),xvalues=c(.10,.20,.35,.40,.50),policy_id=NULL,rbl=TRUE,plot=TRUE)
    {
       # Returns values to create Adverse Selection plot invented by Glenn Meyers
        #     x :  plot values for the x-axis
        #     lr_increase: The increase in loss ratio caused by competitors 
        #          selecting lower relativities
        #    
        # Plots "The Effect of Competitors Selecting Lower Relativites" graph
        #
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 #  pred_current: a vector of baseline predictions
 
 # Optional Inputs:
 #   weight: a vector of weights set to a vector
 #          of 1's by default
 #   xvalues: A vector of the percent of items selected.  Plotted on the x-axis
 #   policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #   rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #   plot: TRUE if a plot should be produced
 #        FALSE if the plot should be surpressed
    
        ## run data prep code
        # check that vectors are the same length
        if(length(policy_id)==0){
            if(!is_same_length(list(actual,pred_new,pred_current,weight))){
                return("Error: Inputs are not the same length")
            }
        }
        else{
            if(!is_same_length(list(actual,pred_new,pred_current,weight,policy_id))){
                return("Error: Inputs are not the same length")
            }
        }
        
        # rebalance predictions to match actuals if rbl is TRUE
        if(rbl){
            dd <- rebalance(actual,data.frame(pred_new,pred_current),weight)
        }
        else{
            dd <- data.frame(actual,pred_new,pred_current,weight)
        }

        #summarize to policy level if that option is specified
        if(length(policy_id)!=0){
            actual <- dd$actual
            pred_new <- dd$pred_new
            pred_current <- dd$pred_current
            weight <- dd$weight
            dd <- policy_level(data.frame(actual,pred_new,pred_current),weight,policy_id)
        }

    # calculate variables from data.frame dd, then remove dd
    dd$relativity <- dd$pred_new / dd$pred_current
    dd$wa <- dd$actual*dd$weight 
    dd$wc <- dd$pred_current*dd$weight
    avg_lr <- sum(dd$wa) / sum(dd$wc)
    l <- length(dd$relativity)
    dd_sorted_des <- dd[order(dd$relativity,decreasing=TRUE),]
    rm(dd)

    # create "Effect of Selecting Lower Relativities" graph
    # sort the xvalues in increaseing order
    x <- xvalues[order(xvalues)]
    # calculate the loss ratio decrease for each xvalue
    lr_increase <- vector(length=length(x))
    for (i in 1:length(x)){
        lr_increase[i] <- (sum(dd_sorted_des[1:I((1-x[i])*l),"wa"]) / sum(dd_sorted_des[1:I((1-x[i])*l), "wc"])) / avg_lr - 1  
    }
        
    if(plot==TRUE)
        {
        windows()
        barplot(lr_increase*100
            ,xlab="% Items Lost to Competition"
            ,ylab="% Increase in Loss Ratio"
            ,names.arg=x*100
            ,main="Effect of Competitors Selecting Lower Relativites"
            )
        }
    plot_values <- data.frame(x,lr_increase)
    return(plot_values)
    }

#value of lift 
value_of_lift <- function(actual,pred_new,pred_current,weight=rep(1,length(actual)),policy_id=NULL,rbl=TRUE,plot=TRUE)
    {
        # Returns the value of lift metric invented by Glenn Meyers
        # he value of lift is the $ per car year value of adopting the new model
        
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 #  pred_current: a vector of baseline predictions
 
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #          of 1's by default
 #  policy_id: a vector that contains policy numbers
 #       set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #       to have the same average as the actual values
 #       FALSE if the predictions should not be rebalanced
 #  plot: TRUE if a plot should be produced
 #       FALSE if the plot should be surpressed
    
        ## run data prep code
        # check that vectors are the same length
        if(length(policy_id)==0){
            if(!is_same_length(list(actual,pred_new,pred_current,weight))){
                return("Error: Inputs are not the same length")
            }
        }
        else{
            if(!is_same_length(list(actual,pred_new,pred_current,weight,policy_id))){
                return("Error: Inputs are not the same length")
            }
        }
        
        # rebalance predictions to match actuals if rbl is TRUE
        if(rbl){
            dd <- rebalance(actual,data.frame(pred_new,pred_current),weight)
        }
        else{
            dd <- data.frame(actual,pred_new,pred_current,weight)
        }

        #summarize to policy level if that option is specified
        if(length(policy_id)!=0){
            actual <- dd$actual
            pred_new <- dd$pred_new
            pred_current <- dd$pred_current
            weight <- dd$weight
            dd <- policy_level(data.frame(actual,pred_new,pred_current),weight,policy_id)
        }

    # create meyers vars
    dd$relativity <- dd$pred_new / dd$pred_current
    risks_lost <- subset(dd,relativity < 1)

    dd$wa <- dd$actual*dd$weight
    dd$wc <- dd$pred_current*dd$weight

    risks_lost$wa <- risks_lost$actual*risks_lost$weight
    risks_lost$wc <- risks_lost$pred_current*risks_lost$weight
    risks_lost$wp <- risks_lost$pred_new*risks_lost$weight

    vol <- (sum(dd$wa) / sum(dd$wc) - sum(risks_lost$wa) / sum(risks_lost$wc)
               )* sum(risks_lost$wc) / sum(dd$weight)
    return(vol)
    }

  
rs_lorenz_gini  <- function(actual, pred_new, pred_current, method = "exposure",
  weight = rep(1, length(actual)), n_sample = length(actual), 
  n_iter = 250, policy_id=NULL, rbl = TRUE, plot = TRUE){
 # Returns a vector of the Gini index from random samples of 
 # the data set.
 # 
 # Inputs:
 #  actual: a vector of actual values
 #  pred_new: a vector of predicted values
 #  pred_current: a vector of predictions from current model
 #  method: can be either 'Exposure' or 'Premium'
 # Optional Inputs:
 #  weight: a vector of weights set to a vector
 #   of 1's by default
 #  n_sample: number of samples in each iteration (defaults to length of actual)
 #  n_iter: number of iterations (defualts to 250)
 #  policy_id: a vector that contains policy numbers
 #        set by default to NULL
 #  rbl: TRUE if the predictions should be rebalanced
 #        to have the same average as the actual values
 #        FALSE if the predictions should not be rebalanced
 #  plot: TRUE if a plot should be produced
 #        FALSE if the plot should be surpressed

 ## Check if actual, pred_new and weight have the same length
 if(!is_same_length(list(actual, pred_new, pred_current, weight))){
  return("Error: Inputs are not the same length")
 }
 
 # rebalance predictions to match actuals if rbl is TRUE
 if(rbl){
  dd <- rebalance(actual,data.frame(pred_new,pred_current),weight)
 }
 else{
  dd <- data.frame(actual,pred_new,pred_current,weight)
 }
 
 #summarize to policy level if that option is specified
 if(length(policy_id)!=0){
  actual <- dd$actual
  pred_new <- dd$pred_new
  pred_current <- dd$pred_current
  weight <- dd$weight
  dd <- policy_level(data.frame(actual,pred_new,pred_current),weight,policy_id)
 }
 
 ## Initialize return vector
 gini <- rep(0, n_iter)
 
 for (i in 1:n_iter){
  ## sample original data set
  rows <- sample(1:length(dd$actual), n_sample, replace = TRUE)
  temp <- dd[rows, ]
  
  ## Call the Lorenz_Gini function
  z <- Lorenz_Gini(actual = temp$actual, 
    pred_new = temp$pred_new, 
    pred_current = temp$pred_current,
    weight = temp$weight,
    method = method,
    rbl = FALSE,
    plot = FALSE)
  gini[i] <- z$Gini_index

 }
 if (plot == TRUE){
  windows()
  
  plot_main <- 'Random Sample Distribution'
  plot_sub <- 'Gini Index'
  y_text <- 'Gini Index'
  boxplot(gini, 
    main = plot_main,
    sub = plot_sub,
    ylab = y_text)
 }
 
 return(gini)
}

########################################
# Program Name:   twoWayLift.R
# Project Name:   Loss Cost Diagnostics
# 
# Purpose: Create two way Lift Charts
#
# Data Description: Simulated Tweedie data
#
# References:  
#
# Usage: See function inputs and outputs 
########################################
# input is 4 vectors of equal length, number of tiles and a boolean
    # actual is the observed loss costs
    # pred_new1 is the proposed loss cost predictions based on model 1
    # pred_new2 is the proposed loss cost predictions based on model 2
    # weight is exposures, default to 1
    # tile is number of tiles requested
    # plot is TRUE of FALSE
    
    # Optional Inputs:
    #    policy_id: a vector that contains policy numbers
    #                set by default to NULL
    #    rbl: TRUE if the predictions should be rebalanced
    #               to have the same average as the actual values
    #         FALSE if the predictions should not be rebalanced

# output is a two-waylift chart plot and a data frame of lift curve values

two_way_lift <- function(actual,pred_new1, pred_new2, weight = rep(1,length(actual)), tile = 10, policy_id=NULL, rbl=TRUE, plot=TRUE) { 
    # Returns a data frame of lift curves values
    # 
    # Inputs: 
    #  actual: a vector of actual values
    #  pred_new1: a vector of predicted values based on model 1
    #  pred_new2: a vector of predicted values based on model 2
    #  weight: a vector of weights, default to 1
    #  tile: number of tiles requested
    
    # Optional Inputs:
    #    policy_id: a vector that contains policy numbers
    #                set by default to NULL
    #    rbl: TRUE if the predictions should be rebalanced
    #         to have the same average as the actual values
    #         FALSE if the predictions should not be rebalanced

        ## run data prep code
        # check that vectors are the same length
        if(length(policy_id)==0){
            if(!is_same_length(list(actual,pred_new1,pred_new2,weight))){
                return("Error: Inputs are not the same length")
            }
        }
        else{
            if(!is_same_length(list(actual,pred_new1,pred_new2,weight,policy_id))){
                return("Error: Inputs are not the same length")
            }
        }
        
        # rebalance predictions to match actuals if rbl is TRUE
        if(rbl){
            dd <- rebalance(actual,data.frame(pred_new1,pred_new2),weight)
        }
        else{
            dd <- data.frame(actual,pred_new1,pred_new2,weight)
        }

        #summarize to policy level if that option is specified
        if(length(policy_id)!=0){
            actual <- dd$actual
            pred_new1 <- dd$pred_new1
            pred_new2 <- dd$pred_new2
            weight <- dd$weight
            dd <- policy_level(data.frame(actual,pred_new1,pred_new2),weight,policy_id)
        }
 
    dd$ratio <- dd$pred_new1 / dd$pred_new2

    o<- order(dd$ratio)

    dd<-dd[o,]

    n = nrow(dd)

    dd$cumsum <- cumsum(dd$weight)
    tiles <- ceiling((dd$cumsum / dd$cumsum[n]) * tile)
   
   #take care of unassigned last decile due to rounding issue
   if (tiles[n] == 0) {
           tiles[n]=tile
    }

   dd$tiles<- tiles

   ddx <- split(dd, dd$tiles)
 
   m_1 <- sapply(ddx, function(z) weighted.mean(z$pred_new1, w = z$weight))
   m_2 <- sapply(ddx, function(z) weighted.mean(z$pred_new2, w = z$weight)) 
   m   <- sapply(ddx, function(z) weighted.mean(z$actual,w = z$weight))
 

   theory_pred_new1 <- (m / m_1)  
   theory_pred_new2 <- (m / m_2)

   mean1 <- mean(theory_pred_new1)
   mean2 <- mean(theory_pred_new2)

   theory_pred_new1 <-  theory_pred_new1 / mean1 
   theory_pred_new2 <-  theory_pred_new2 / mean2

   #abs_dist_pred1 <- abs(theory_pred_new1 - 1)
   #abs_dist_pred2 <- abs(theory_pred_new2 - 1)
   #sqr_dist_pred1 <- (theory_pred_new1 - 1) ^ 2
   #sqr_dist_pred2 <- (theory_pred_new2 - 1) ^ 2

   pct<-seq(1:tile)

   if(plot==TRUE)
       { 
       windows()
       plot(pct, theory_pred_new2, xlim = c(1,tile), 
                           ylim=c(0,max(theory_pred_new1, theory_pred_new2) ), 
                           xlab= 'Decile',
                           ylab= 'LR Relativity', 
                           main = 'Two way lift chart: Model 1 vs. Model 2',
                           type="n",
       lab=c(10,5,7)
     )
       abline(h=1)
       lines(pct, theory_pred_new1, lty =1, lwd=1, col='blue')
       lines(pct, theory_pred_new2, lty =2, lwd=2, col='red')
       legend.txt <- c('Model 1', 'Model2')
       legend("topleft", legend= legend.txt, 
           col = c(1,2),
           lty = c(1,2),
           lwd = c(1,2),
           bty = "o", cex = 0.8)
       }

    lrr1 <- theory_pred_new1
    lrr2 <- theory_pred_new2
    plot_values <- data.frame(pct,lrr1,lrr2)
    return(plot_values)
 
}

###########################################################
# Helper Functions 
#
#check vector lengths
is_same_length <- function(list_of_vectors){
 # Returns TRUE if all the vectors in a list
 # of vectors are the same length, and returs
 # false otherwise.
 #
 # Input: list_of_vectors
 #  
 # > v <- c(list(actual, pred_new, weight))
 # > is_same_length(v)
 
 ## Get the length of the first vector
 len_vector <- length(list_of_vectors[[1]])
 
 ## set the indicator to TRUE
 is_same <- TRUE
 
 ## Loop through the remaining vectors
 for (i in 2:length(list_of_vectors)){  
  ## Check if the candidate vector is the same length 
  ## as the first vector.
  if(len_vector != length(list_of_vectors[[i]])){
   is_same <- FALSE
  }
 }
 return(is_same)
}

## rebalance predictions to match actuals
rebalance <- function(actual,dataframe_of_preds,weight=rep(1,length(actual)))
    {
    # Returns a dataframe of actuals, rebalanced predictions, and weight
 #
 # Inputs:
    #     actual: a vector of actual values
    #     dataframe_of_preds: a dataframe of predictions
    #     weight: a vector of weights
 #  
 # > df <- data.frame(pred_1,pred_2,..,pred_N)
 # > rebalance(actual,df,weight)

    rebalanced_preds <- data.frame(matrix(nrow=nrow(dataframe_of_preds),ncol=ncol(dataframe_of_preds)))
    ## Loop through the set of predictions
    for (i in 1:ncol(dataframe_of_preds))
        {
   # rebalance predictions to match actuals
        rebalanced_preds[,i] <- dataframe_of_preds[,i]*weighted.mean(actual,weight)/weighted.mean(dataframe_of_preds[,i],weight)
        names(rebalanced_preds)[i] <- names(dataframe_of_preds)[i]
   }
    dd <- data.frame(actual,rebalanced_preds,weight)
    return(dd)
    }

## summarize observations to policylevel
policy_level <- function(dataframe_of_obs,weight=rep(1,length(actual)),policy_id)
    {
     # Returns a dataframe of actuals, rebalanced predictions, and weight
 #
 # Inputs:
        #     dataframe_of_obs: a dataframe of observations to be rolled up to a policylevel
        #     weight: a vector of weights
        #     policy_id: a vector of unique policy identifiers
 # Usage: 
 # > df <- data.frame(actual,pred_1,pred_2,...,pred_N)
 # > policy_level(df,weight,policy_id)

    aw <- aggregate(weight,by=list(policy_id),FUN=sum)$x
    policylevel_obs <- data.frame(matrix(nrow=length(aw),ncol=length(dataframe_of_obs)))
    
    # Loop through the vectors
    for (i in 1:length(dataframe_of_obs))
        {
   # summarize to policy level
        wo <- dataframe_of_obs[i]*weight
        policylevel_obs[i] <- aggregate(wo[,1],by=list(policy_id),FUN=sum)$x / aw
        names(policylevel_obs)[i] <- names(dataframe_of_obs)[i]
   }
    weight <- aw
    policy_id <- unique(policy_id)
    dd <- data.frame(policylevel_obs,weight,policy_id)
    return(dd)
    }

# create a vector of deciles
decile <- function(obs,weight=rep(1,length(obs[1])),n_tile=10)
    {
       # Returns a vector of deciles that can be merged onto the input data
 #
 # Inputs:
        #     obs: a vector used for sorting and deciling on
        #     weight: a vector of weights
        #     n_tile: the number of tiles desired
        #           defaults to 10
        #
        # Tiles are weighted tiles

    index <- 1:length(obs)
    dd <- data.frame(obs,weight,index)
    dd_sorted <- dd[order(dd$obs),]

    ## create weighted tile
    n=nrow(dd_sorted)
    dd_sorted$cumsum <- cumsum(dd_sorted$weight)
    tile <- ceiling((dd_sorted$cumsum / dd_sorted$cumsum[n]) * n_tile)
 
    #take care of unassigned last decile due to rounding issue
    if (tile[n] == 0) {
        tile[n]=n_tile
        }
    dd_sorted$tile <- tile
    dd_unsorted <- dd_sorted[order(dd_sorted$index),]
    return(dd_unsorted$tile)    
    }

