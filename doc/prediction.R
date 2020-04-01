# library packages
library(R.matlab)
library(readxl)
library(dplyr)
library(EBImage)
library(writexl)
library(tibble)

# set path
path <- getwd()
setwd(path)

# direction to test set
test_dir <- "../data/test_set_predict/" # This will be modified for different data sets.
test_image_dir <- paste(test_dir, "images/", sep="")
test_pt_dir <- paste(test_dir,  "points/", sep="")
test_label_path <- paste(test_dir, "label.csv", sep="") 

# source functions needed
source("../lib/featurefortest.R")
source("../lib/train.R")
source("../lib/test.R")

load("../output/baseline_model.RData")    # fit_train_base
load("../output/model.RData")             # fit_train  

# load table and images
n_files <- length(list.files(test_image_dir))

image_list <- list()
for(i in 1:100){
  image_list[[i]] <- readImage(paste0(test_image_dir, sprintf("%04d", i), ".jpg"))
}

readMat.matrix <- function(index){
  return(round(readMat(paste0(test_pt_dir, sprintf("%04d", index), ".mat"))[[1]],0))
}

#load fiducial points
fiducial_pt_list <- lapply(1:n_files, readMat.matrix)

#feature extraction
tm_feature <- NA
idx <- 1:n_files
tm_feature <- system.time(dat <- featuretest(fiducial_pt_list, idx))


# baseline
tm1 <- system.time(pred1 <- test(fit_train_base,dat, run.gbm = T))

# improved
tm2 <- system.time(pred2 <- test(fit_train,dat, run.svm = T))

time <- tibble(tm_feature=tm_feature, Baseline = tm1, Advanced = tm2)
time

t <- read.csv(paste(test_dir,"labels_prediction.csv",sep=""))
t<-t%>%select(-X)
t$Baseline <- pred1
t$Advanced <- pred2
write.csv(t,file = "../output/labels_prediction.csv")



