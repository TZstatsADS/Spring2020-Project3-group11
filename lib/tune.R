##########################
### Turning Parameters ###
##########################


tune <- function(dat_train,
                 label_train,
                 run.gbm = F,
                 run.xgboost = F,
                 run.adaboost = F,
                 verbose = FALSE){
  
  ### tune parameter
  
  ### Input: 
  ###   dat_train -  processed features from images 
  ###   label_train -  class labels for training images
  ###   run.xxxxxx - select models (gbm is the baseline)
  ###   verbose - TRUE means print cv error while every loop
  
  ### Output: 
  ###   best parameter
  
  ### load libraries
  library("gbm")
  library("adabag")
  library("xgboost")
  
  ### load functions 
  source("../lib/cross_validation.R")
  
  
  
  
  ## gbm model tune parameter
  
  if(run.gbm == T){
    
    ## parameter pool
    shrinks_range <- c(0.01,0.05,0.1,0.15,0.2)
    trees_range  <- c(40,50,60,70,100)
    ## initial cv error
    error_matrix = matrix(NA,nrow = length(shrinks_range), length(trees_range))
    rownames(error_matrix) <- paste(shrinks_range)
    colnames(error_matrix) <- paste(trees_range)
    ## initial cv sd
    sd_matrix = matrix(NA,nrow = length(shrinks_range), length(trees_range))
    rownames(sd_matrix) <- paste(shrinks_range)
    colnames(sd_matrix) <- paste(trees_range)
    
    ## loop parameter combination
    for (i in 1:length(shrinks_range)){
      for (j in 1:length(trees_range)){
        par <- list(shrinkage = shrinks_range[i], ntrees = trees_range[j] )
        error_matrix[i,j] <- cv(dat_train, label_train, run.gbm= T, par = par)$error
        sd_matrix[i,j] <- cv(dat_train, label_train, run.gbm= T, par = par)$sd
      }
    }
    
    # best cv.error
    cv_error =  min(error_matrix)
    
    # best parameter
    best_par = list(shrinkage = shrinks_range[which(error_matrix == min(error_matrix), arr.ind = T)[1]],
                    ntrees = trees_range[which(error_matrix == min(error_matrix), arr.ind = T)[2]])
  }
  
  
  
  ## adaboost
  
  if(run.adaboost == T){
    ## parameter pool
    mfinal <- c(50, 75, 100, 125)
    
    ## initial cv error
    cv.error <- c()
    
    ## loop parameter combination
    for(i in length(mfinal)){
      par <- list(mfinal = mfinal[i])
      cv.error[i] <- cv(dat_train, label_train, run.adaboost = T, par = par)$error
    }
    
    # best cv.error
    cv_error =  min(cv.error)
    # best parameter
    best.mfinal = mfinal[which(cv.error == min(cv.error))]
    best_par = list(mfinal = best.mfinal)
    
  }
  
  
  
  ## xgboost model tune parameter
  
  if(run.xgboost == T){
    
    max_depth_values <- seq(3,9,2)
    min_child_weight_values <- seq(1,6,2)
    
    # error matrix
    error_matrix = matrix(NA,nrow = length(max_depth_values), length(min_child_weight_values))
    rownames(error_matrix) <- paste(max_depth_values)
    colnames(error_matrix) <- paste(min_child_weight_values)
    
    ## cv sd matrix
    sd_matrix = matrix(NA,nrow = length(max_depth_values), length(min_child_weight_values))
    rownames(sd_matrix) <- paste(max_depth_values)
    colnames(sd_matrix) <-  paste(min_child_weight_values)
    
    #tuning process
    for (i in 1:length(max_depth_values)){
      for (j in 1:length(min_child_weight_values)){
        par <- list(depth = max_depth_values[i], child_weight = min_child_weight_values[j] )
        error_matrix[i,j] <- cv(dat_train, label_train, run.xgboost = T, par = par)$error
        sd_matrix[i,j] <- cv(dat_train, label_train, run.xgboost = T, par = par)$sd
      }
    }
    
    # best cv.error
    cv_error =  min(error_matrix)
    # best parameter
    best_par = list(depth = max_depth_values[which(error_matrix == min(error_matrix), arr.ind = T)[1]],
                    child_weight = min_child_weight_values[which(error_matrix == min(error_matrix), arr.ind = T)[2]])
    
  }
  
  if(verbose == TRUE){
    print(error_matrix)
    print(sd_matrix)
  }
  
  return(list(cv_error,best_par))
}