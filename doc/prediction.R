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
test_dir <- "../data/test_set/" # This will be modified for different data sets.
test_image_dir <- paste(test_dir, "images/", sep="")
test_pt_dir <- paste(test_dir,  "points/", sep="")
test_label_path <- paste(test_dir, "label.csv", sep="") 

# source functions needed
source("../lib/featurefortest.R")
source("../lib/train.R")
source("../lib/test.R")

source("../output/baseline_model.RData.R")    # fit_train_base
source("../output/model.RData.R")             # fit_train  

# load table and images
info <- read.csv(test_label_path)

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
tm1 <- system.time(pred1 <- test(fit_train_base,dat))

# improved
tm2 <- system.time(pred2 <- test(fit_train,dat))

time <- tibble(tm_feature=tm_feature, Baseline = tm1, Advanced = tm2)
time

t <- read.csv(paste(test_dir,"labels_prediction.csv",sep=""))
t$Baseline <- pred1
t$Advanced <- pred2
write.csv(t,file = "../output/labels_prediction.xlsx")



