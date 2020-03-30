####################################################################
### Train a selected classification model with training features ###
####################################################################

train <- function(dat_train,             # a dataframe with feature without label
                  label_train,           # the label of dat_train
                  run.gbm = F,           # selection 1
                  run.xgboost = F,       # selection 2
                  run.adaboost = F,      # selection 3
                  run.ksvm = F,
                  par = NULL             # parameter set
                  ){
  
  ### Input:
  ### dat_train - a dataframe with feature without label
  ### label_train - class labels for training images
  
  ### below are model selections, the default is not to train with the models
  ### run.xgbm - selection 1 gbm (baseline)
  ### run.xgboost - selection 2
  ### run.adaboost - selection 3
  
  ### Output
  ### the fitted model of the selected models
  
  
  
  ### fit selected model
  
  
  #### gradient boosting model
  
  if(run.gbm == T){
    library("gbm")
    if(is.null(par)){
      ntrees = 100
      shrinkage = 0.1
    }
    else{
      ntrees = par$ntrees
      shrinkage = par$shrinkage
    }
    fit.model <- gbm.fit(x = dat_train,
                         y = label_train,
                         interaction.depth = 3, 
                         shrinkage = shrinkage,
                         bag.fraction = 0.5,
                         n.trees = ntrees,
                         verbose = FALSE,
                         distribution="multinomial")
  }

  
  
  
  ####  adaboosting model
  
  if(run.adaboost == T){
    library("adabag")    
    # load parameter
    if(is.null(par)){
      mfinal <- 100
    } else {
      mfinal <- par$mfinal
    }
    
    # convert trainning data to data frame
    train <- data.frame(label = factor(label_train), 
                        data = dat_train)
    
    # fit model
    fit.model <- boosting(label~.,data = train,
                          mfinal = mfinal, 
                          coeflearn= "Zhu")
  }
  
  
  
  #### xgboost model
  
  if(run.xgboost == T){
    library("xgboost")
    if(is.null(par)){
      depth <- 5
      child_weight <- 3
    } else {
      depth <- par$depth
      child_weight <- par$child_weight
    }
    
    # create xgb.Dmatrix
    train_matrix <- xgb.DMatrix(data=data.matrix(dat_train),
                                label=as.numeric(label_train)-1)
    
    # fit xgboost model
    fit.model <- xgb.train(data = train_matrix,
                           max.depth = depth,
                           min_child_weight = child_weight,
                           eta = 0.3,
                           nthread = 4,
                           nround = 100,
                           num_class = 22)
  }
  
  
  #### ksvm model
  if(run.ksvm == T){
    library("kernlab")
    # load parameter
    if(is.null(par)){
      kernel <- "rbfdot"
      C <- 1
    } else {
      kernel <- par$kernel
      C <- par$C
    }
    
    # convert trainning data to data frame
    train <- as.matrix(dat_train)
    
    # fit model
    fit.model <- ksvm(as.matrix(train),
                 label_train,
                 kernel=kernel,
                 kpar="automatic)",
                 cross=5,
                 C = C)
  }
  
  return(fit.model)
  
}

