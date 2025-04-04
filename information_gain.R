## run entropy function first
infomation_gain <- function(D){
  n <- nrow(D)
  info_gain <- data.frame()
  for (feature in 1:(ncol(D)-1)){
    values <- unique(D[,feature])
    sum_entr_feature <- 0
    for (v in values){
      set <- which(D[,feature]==v)
      prop <- length(set)/n
      entr_v <- entropy(D[set,ncol(D)])
      entr_v <- prop*entr_v
      sum_entr_feature <- sum_entr_feature + entr_v
    }
    info_gain_feature <- entropy(D[,ncol(D)]) - sum_entr_feature
    info_gain <- rbind(info_gain, info_gain_feature)
    rownames(info_gain)[feature] <- paste('Feature', feature)
  }
  colnames(info_gain) <- c('Information Gain')
  return(info_gain)
}