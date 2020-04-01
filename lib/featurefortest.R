featuretest<-function(input_list = fiducial_pt_list, index){
  pairwise_dist <- function(vec){
    ### input: a vector(length n), output: a vector containing pairwise distances(length n(n-1)/2)
    return(as.vector(dist(vec)))
  }
  
  ### Step 2: Write a function pairwise_dist_result to apply function in Step 1 to column of a matrix 
  pairwise_dist_result <-function(mat){
    ### input: a n*2 matrix(e.g. fiducial_pt_list[[1]]), output: a vector(length n(n-1))
    return(as.vector(apply(mat, 2, pairwise_dist))) 
  }
  
  ### Step 3: Apply function in Step 2 to selected index of input list, output: a feature matrix with ncol = n(n-1) = 78*77 = 6006
  pairwise_dist_feature <- t(sapply(input_list[index], pairwise_dist_result))
  colnames(pairwise_dist_feature) <- paste("feature", 1:(ncol(pairwise_dist_feature)), sep = "")
  pairwisedata<-as.data.frame(pairwise_dist_feature)
  return(feature_df=pairwisedata)
}