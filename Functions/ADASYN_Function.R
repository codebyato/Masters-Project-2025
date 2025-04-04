library(FNN)


ADASYN_function<- function(df,k,beta){
  minority <- data.frame()
  majority <- data.frame()
  
  for (i in 1:nrow(df)){
    if (df[i,ncol(df)] ==1 ){
      minority <- rbind(minority, df[i,(1:2), drop=FALSE])
    }
    if (df[i,ncol(df)]==0){
      majority <- rbind(majority,df[i,(1:2),drop=FALSE])
    }
  }
  
  ratios <- data.frame()
  new_ratios <- data.frame()
  
  G <- (nrow(majority) - nrow(minority)) * 1
  for ( l in 1:nrow(minority)){
    nghbr_in_maj <-0
    k_neighbours <- get.knnx(data = df[,1:2],query = minority[l,], k= k)
    nhbr_index <- k_neighbours$nn.index
    for ( j in 1:length(nhbr_index)){
      if (df[nhbr_index[j], ncol(df), drop=FALSE] == 0){
        nghbr_in_maj <- nghbr_in_maj + 1
      }
    }
    r_i <- (nghbr_in_maj/k)
    ratios <- rbind(ratios, r_i)
  }
  sum_ratios <- sum(ratios)
  for (m in 1:nrow(ratios)){
    r_m <- ratios[m,]
    if( r_m > 0 ){
      r_i_hat <- (r_m / sum_ratios)
    }
    else {
      r_i_hat <-0
    }
    new_ratios <- rbind(new_ratios,r_i_hat)
  }
  
  superficial <- data.frame()
  for (x in 1:nrow(minority)){
    g_i <- round(G * (new_ratios[x,]))
    if (g_i > 0){
      k_neighbours <- get.knnx(data = minority[-x,],query = minority[x,], k= k)
      rname <- rownames(minority)[x]
      for (y in 1:g_i){
        index <- sample(k_neighbours$nn.index,1)
        k_nhbr <- minority[index,,drop = FALSE]
        newPoint <- minority[x,] + runif(1) * (k_nhbr - minority[x,])
        newPoint <- cbind(newPoint,1)
        colnames(newPoint) <- colnames(df)
        superficial <- rbind(superficial, newPoint)
        rownames(superficial)[nrow(superficial)] <- paste(rname,'syn=',y)
      }
    } 
  }
  
  
  df <- rbind(df,superficial)
  return(df)
}

