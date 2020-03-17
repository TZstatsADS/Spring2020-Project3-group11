####################################################################
### Train a selected classification model with training features ###
####################################################################

train <- function(dat_train,             # a dataframe with feature without label
                  label_train,           # the label of dat_train
                  run.gbm = F,           # selection 1
                  run.xgboost = F,       # selection 2
                  run.adaboost = F,      # selection 3
                  par = NULL             # parameter set
                  ){
  
  ### Input:
  ### dat_train - a dataframe with feature without label
  ### label_train - the label of dat_train
  
  ### below are model selections, the default is not to train with the models
  ### run.xgbm - selection 1 gbm (baseline)
  ### run.xgboost - selection 2
  ### run.adaboost - selection 3
  
  ### Output
  ### the fitted model of the selected models
  
  library("gbm")
  library("adabag")
  library("xgboost")
  
  
  ### fit selected models
  
  
  #### gradient boosting model
  
  if(run.gbm == T){
    
    if(is.null(par)){
      ntrees = 100
      shrinkage = 0.1
    }
    else{
      ntrees = par$ntrees
      shrinkage = par$shrinkage
    }
    fit.model <- gbm.fit(x = dat_train[,-ncol(dat_train)],
                         y = dat_train[,ncol(dat_train)],
                         interaction.depth = 3, 
                         shrinkage = shrinkage,
                         bag.fraction = 0.5,
                         n.trees = ntrees,
                         verbose = FALSE,
                         distribution="multinomial")
  }
  
  
  ####  adaboost model
  
  if(run.adaboost == T){
    
    # load parameter
    if(is.null(par)){
      mfinal <- 100
    } else {
      mfinal <- par$mfinal
    }
    
    # convert trainning data to data frame
    train <- data.frame(label = factor(dat_train[,ncol(dat_train)]), 
                        data = dat_train[,-ncol(dat_train)])
    
    # fit model
    fit.model <- boosting(label~.,data = train,
                          mfinal = mfinal, 
                          coeflearn= "Zhu")
  }
  
  
  
  #### xgboost model
  
  if(run.xgboost == T){
    if(is.null(par)){
      depth <- 5
      child_weight <- 3
    } else {
      depth <- par$depth
      child_weight <- par$child_weight
    }
    
    # create xgb.Dmatrix
    train_matrix <- xgb.DMatrix(data=data.matrix(dat_train[,-ncol(dat_train)]),
                                label=as,numeric(dat_train[,ncol(dat_train)])-1)
    
    # fit xgboost model
    fit.model <- xgb.train(data = train_matrix,
                           max.depth = depth,
                           min_child_weight = child_weight,
                           eta = 0.3,
                           nthread = 4,
                           nround = 100,
                           num_class = 22)
  }
  return(fit.model)
  
}

