#############################################################
### Construct features and responses for training images  ###
#############################################################

feature_dist <- function(input_list = fiducial_pt_list, index){
  
  ### Construct process features for training images 
  
  ### Input: a list of images or fiducial points; index: train index or test index

  ### Output: a data frame containing: features and a column of label
  
  ### here is an example of extracting pairwise distances between fiducial points
  ### Step 1: Write a function pairwise_dist to calculate pairwise distance of items in a vector
  pairwise_dist <- function(mat){
    ### input: a vector(length n), output: a vector containing pairwise distances(length n(n-1)/2)
    m1 <- c(mat[,1])
    m2 <- c(mat[,2])
    m <- data_frame(m1=m1,m2=m2)
    return(as.vector(dist(m)))
  }
  
  ### Step 2: Apply function in Step 2 to selected index of input list, output: a feature matrix with ncol = n(n-1) = 78*77 = 6006
  pairwise_dist_feature <- c()
  for(i in 1:length(index)){
    pairwise_dist_feature <- rbind(pairwise_dist_feature,pairwise_dist(input_list[[i]]))}
  dim(pairwise_dist_feature) 
  
  ### Step 4: construct a dataframe containing features and label with nrow = length of index
  ### column bind feature matrix in Step 3 and corresponding features
  pairwise_data <- cbind(pairwise_dist_feature, info$emotion_idx[index])
  ### add column names
  colnames(pairwise_data) <- c(paste("feature", 1:(ncol(pairwise_data)-1), sep = ""), "emotion_idx")
  ### convert matrix to data frame
  pairwise_data <- as.data.frame(pairwise_data)
  ### convert label column to factor
  pairwise_data$emotion_idx <- as.factor(pairwise_data$emotion_idx)
  
  return(feature_df = pairwise_data)
}
