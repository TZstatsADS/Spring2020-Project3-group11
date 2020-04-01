###########
## KPCA ###
###########

kpca_f <- function(dat_train,
                 dat_test,
                 run.kpca=F){
  if(run.kpca==T){
    library(kernlab)
    kpca_model <- kernlab::kpca(data = dat_train, kernel = "vanilladot", features=2)
    p.train <- pcv(kpca_model)
    p.test <- predict(kpca_model, dat_test)
  }
  return(list(p_train = p.train, 
              p_test = p.test))
}
