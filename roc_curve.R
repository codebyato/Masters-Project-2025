library(flux)

## before running must run sens_vs_spec function

roc_curve <- function(predictions,df){
  table4 <- vector()
  for (i in seq(from=0, to=10, by=1)){
    i <- i/10
    table4 <- rbind(table4,sens_vs_spec(i,predictions,df))
  }
  table4<- data.frame(table4)
  colnames(table4)<- c('sensitivity','specificity')
  x<- (1-table4$`specificity`)
  y <- table4$`sensitivity`
  AUC <- auc(x,y)
  similarity_name <- strsplit(deparse(substitute(df)), "_")[[1]]
  plot(x,y,xlim=c(0,1),
       ylim= c(0,1), type="l", col="black", lwd=3, xlab= 'False Positive Rate',ylab = 'True Positive Rate', 
       main = paste(similarity_name[1],'ROC Curve', sep =" "))
  text(0.3,0.6, label= paste('AUC Value =',round(AUC,4), sep = ' '), cex = 0.75)
  abline(0,1)
  
  return(AUC)
}
