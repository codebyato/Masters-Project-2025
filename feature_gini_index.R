feature_gini_index <- function(D,condition){
  n <- nrow(D)
  feat_gini_index <- data.frame()
  for (feature in 1:(ncol(D)-1)){
    x_1 <- which(D[,feature] > condition)
    x_2 <- which(D[,feature] <= condition)
    n_1 <- length(x_1)
    n_2 <- length(x_2)
    gini_idx <- (n_1/n)*gini_index(D[x_1,ncol(D)]) + (n_2/n)*gini_index(D[x_2,ncol(D)])
    feat_gini_index <- rbind(feat_gini_index,gini_idx)
    rownames(feat_gini_index)[feature] <- paste('Feature', feature)
  }
  colnames(feat_gini_index) <- c('Gini Index')
  return(feat_gini_index)
}
