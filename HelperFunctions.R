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

