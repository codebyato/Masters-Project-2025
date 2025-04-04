entropy <- function(x){
  classes <- unique(x)
  entr <- 0
  for (i in classes){
    p <- length(which(x==i))/length(x)
    entr <- entr + (-p *log2(p))
  }
  return(entr)
}
