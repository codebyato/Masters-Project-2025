library(FNN)

SMOTE <- function(df,k,N){
  k_neighbours <- data.frame()
  superficial <- data.frame()
  minority <- data.frame()
  N <- N/100
  
  for (i in 1:nrow(df)){
    if (df[i,3] == 1){
      minority <- rbind(minority,df[i,, drop=FALSE])
    }
  }
  
  for ( i in 1:nrow(minority)){
    k_neighbours <- get.knnx(data = minority[-i,],query = minority[i,], k= k)
    rname <- rownames(minority)[i]
    
    for (j in 1:N){
      nhbr_index <- sample(k_neighbours$nn.index,1)
      k_nhbr <- minority[nhbr_index,,drop = FALSE]
      newPoint <- minority[i,] + runif(1) * (k_nhbr - minority[i,])
      newPoint[,3] <- 1
      superficial <- rbind(superficial, newPoint)
      rownames(superficial)[nrow(superficial)] <- paste(rname,'syn=',j)
    }
    
  }
  colnames(superficial) <- colnames(minority)
  
  df <- rbind(df,superficial)
  return(df)
}

