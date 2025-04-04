## run entropy function first 

gain_ratio <- function(D){
  n <- nrow(D)
  gain_rat <- data.frame()
  for (feature in 1:(ncol(D)-1)){
    values <- unique(D[,feature])
    sum_entr_feature <- 0
    split_ratio <-0
    for (v in values){
      set <- which(D[,feature]==v)
      prop <- length(set)/n
      entr_v <- entropy(D[set,ncol(D)])
      entr_v <- prop*entr_v
      sum_entr_feature <- sum_entr_feature + entr_v
      split_ratio <- split_ratio + (length(set)/n)* log2(length(set)/n)
    }
    info_gain_feature <- entropy(D[,ncol(D)]) - sum_entr_feature
    ratio <- info_gain_feature/ -(split_ratio)
    gain_rat <- rbind(gain_rat, ratio)
    rownames(gain_rat)[feature] <- paste('Feature', feature)
  }
  colnames(gain_rat) <- c('Gain Ratio')
  return(gain_rat)
}
