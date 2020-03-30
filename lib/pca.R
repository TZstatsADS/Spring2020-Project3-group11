###########
### PCA ###
###########

pca <- function(dat_train,
                dat_test,
                run.pca=F,
                rank=100){
  if(run.pca==T){
    pc <- prcomp(dat_train, scale = T, rank. = rank)
    p_train <- pc$x
    p_test <- predict(pc,newdata = dat_test)
  }
  return(list(p_train = p_train,
              p_test = p_test))
}