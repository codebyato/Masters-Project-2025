BinaryData <- readRDS('Characteristics_BinaryData.rds')

BUCoefEntry <- Baroni_UrbaniBuser_Function(BinaryData,1,11)
BUCoefTime <- Baroni_UrbaniBuser_Function(BinaryData,12,21)

Pairwise <- Pairwise(BinaryData,5,BUCoefEntry,BUCoefTime)

linked <- data.frame()
for (i in 1:nrow(Pairwise)){
  pair <- Pairwise[i,]
  entry<-pair[1]
  no_entry<- 1- pair[1]
  time_value <- pair[2]
  no_time <- 1- pair[2]
  if (entry > no_entry){
    if (time_value > no_time){
      results <- rownames(pair)
      linked <- rbind(linked,results)
      colnames(linked) <- c('Crime Pair')
    }
  }
}



