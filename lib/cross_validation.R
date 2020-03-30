########################
### Cross Validation ###
########################


cv <- function(dat_train,
               label_train,
               run.gbm = F, 
               run.xgboost = F,
               run.adaboost = F,
               run.ksvm = F,
               K = 5, 
               par = NULL){
  ### Input:
  ### - train data frame
  ### - the label of dat_train
  ### - the baseline model
  ### - other models
  ### - K: a number stands for K-fold CV
  ### - tuning parameters 
  
  ## Output
  ## the error of a set of parameter with cv
  
  n <- dim(dat_train)[1]
  n.fold <- round(n/K, 0)
  set.seed(0)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- dat_train[s != i,]
    test.data <- dat_train[s == i,]
    train.label <- label_train[s != i]
    test.label <- label_train[s == i]
  
    ## choose model
    
    ## gbm (baseline)
    if(run.gbm == T){
      fit.model <- train(train.data, train.label, run.gbm = T, par = par)
      pred <- test(fit.model, test.data, run.gbm = T, par=par)
    }
    
    
    ## adaboost
    if(run.adaboost == T){
      fit.model <- train(train.data, train.label, run.adaboost = T, par = par)
      pred <- test(fit.model, test.data, run.adaboost = T, par = par)
    }
    
    
    ## xgboost
    if(run.xgboost == T){
      fit.model <- train(train.data, train.label, run.xgboost = T, par = par)
      pred <- test(fit.model, test.data, run.xgboost = T, par = par) + 1
    }
    
    
    ## calculate cross validatoin error
    cv.error[i] <- mean(pred != test.label)
    
  }			
  return(list(error = mean(cv.error), sd = sd(cv.error)))
}