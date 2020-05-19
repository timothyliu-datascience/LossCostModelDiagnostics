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
        # Plots "The Effect of Selecting Lower Relativities" graph
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
        # Plots "The Effect of Selecting Lower Relativities" graph
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
        # The value of lift is the $ per car year value of adopting the new model
        
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
 # Returns a vector of the Gini index from random samples of the data set.
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

