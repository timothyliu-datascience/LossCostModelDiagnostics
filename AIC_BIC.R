##########################################################
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
##########################################################

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
 
