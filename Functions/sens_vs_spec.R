sens_vs_spec <- function(x,vector,df){
  predict_real <- vector()
  
  for (j in 1:nrow(vector)) {
    
    if (vector[j,] >= x ) {
      predict_real[j] <- 1
    } else if (vector[j,] < x) {
      predict_real[j] <- 0
    }
  }
  table2 <- cbind(vector,predict_real)
  
  linked <- df[,ncol(df),drop=FALSE]
  
  table3 <- merge(table2,linked, by = 'row.names')
  
  tp <- 0
  tn <- 0
  fn <- 0
  fp <- 0
  
  for (i in 1:nrow(table3)){
    if (table3[i,3] == table3[i,4] & table3[i,4]== 1){
      tp <- tp +1
    }
    if (table3[i,3] == table3[i,4] & table3[i,4] == 0){
      tn <- tn +1
    }
    if (table3[i,3] ==0 & table3[i,4] == 1){
      fn <- fn +1
    }
    if (table3[i,3] ==1 & table3[i,4] == 0){
      fp <- fp +1
    }
  }
  
  sensitivity <- tp /(tp +fn)
  specificity <- tn /(tn + fp)
  sen_spec <- c(sensitivity,specificity)
  
  return(sen_spec)
}
