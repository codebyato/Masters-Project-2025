gini_index <- function(X){
  classes <- unique(X)
  gini <- 0
  for (i in classes){
    p <- length(which(X==i))/length(X)
    gini <- gini + p^2
  }
  return(1- gini)
}
