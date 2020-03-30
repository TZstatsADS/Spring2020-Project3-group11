########################################
### Classification with testing data ###
########################################

test <- function(fit.model, 
                 dat_test,
                 run.gbm = F,
                 run.xgboost = F, 
                 run.adaboost = F, 
                 run.ksvm = F,
                 par=NULL){
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  - processed features from testing images
  ###  - model selection 1 (baseline)
  ###  - other models
  ###  - parameter setting
  
  ### Output: training model predicting on test data
  
  library("gbm")
  library("adabag")
  library("xgboost")
  
  ### make predictions
  
  ## gbm (baseline)
  if (run.gbm ==T){
    if(is.null(par)){
      ntrees = 50
    }else{
      ntrees = par$ntrees
    }
    pred.prob <- predict(fit.model, newdata=as.data.frame(dat_test),n.trees = ntrees,type= "response")
    pred = apply(pred.prob,1,which.max )
  }
  
  
  ## adaboost
  if(run.adaboost == T){
    
    # create test data frame
    test <- data.frame(data = dat_test)
    # predict
    pred = predict.boosting(fit.model,newdata = test)$class
  }
  
 
  ## xgboost
  if(run.xgboost == T){
    if(is.null(par)){
      ntrees = 100
    }else{
      ntrees = par$ntrees
    }
    pred <- predict(fit.model, newdata=as.matrix(dat_test), n.trees = ntrees)
    pred <- pred + 1
  }
  
  
  ## ksvm
  if(run.ksvm == T){
    pred <- predict(fit.model,dat_test)
  }
  
  return(pred)
}
