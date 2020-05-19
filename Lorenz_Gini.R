  
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


