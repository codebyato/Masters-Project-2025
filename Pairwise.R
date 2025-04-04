#Pairwise Table
Pairwise <- function(df,linked_pairs,EntryCoef,TimeCoef){
  
  linked <- vector()
  linked_pair <- vector()
  
  for (i in 1:(nrow(df)-1)){
    for (j in (i+1):nrow(df)){
      size <- linked_pairs
      group_i<- (i-1) %/% size
      group_j <- (j-1) %/% size
      
      if (group_i == group_j){
        linked <- c(linked,1)
        linked_pair <- c(linked_pair, paste0("C", i, " / C", j))
      }
      else{
        linked <- c(linked,0)
        linked_pair <- c(linked_pair, paste0("C", i, " / C", j))
      }
      
    }
  }
  linked <- data.frame(linked)
  rownames(linked) <- linked_pair
  
  Crime_pairwise <- cbind(EntryCoef,TimeCoef,linked)
  colnames(Crime_pairwise)<- c('Coef_Entry_Behaviour', 'Coef_Time','Crime_Pairs')
  Crime_pairwise <- data.frame(Crime_pairwise)
  return(Crime_pairwise)
}
