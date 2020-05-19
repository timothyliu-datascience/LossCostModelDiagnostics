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

 #Formula:
   #theory_pred_new1 <-  theory_pred_new1 / mean1 
   #theory_pred_new2 <-  theory_pred_new2 / mean2

   #abs_dist_pred1 <- abs(theory_pred_new1 - 1)
   #abs_dist_pred2 <- abs(theory_pred_new2 - 1)
   #sqr_dist_pred1 <- (theory_pred_new1 - 1) ^ 2
   #sqr_dist_pred2 <- (theory_pred_new2 - 1) ^ 2

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

