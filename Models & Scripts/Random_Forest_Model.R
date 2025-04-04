library(randomForest)
library(reprtree)
library(caret)

BinaryData <- readRDS('Characteristics_BinaryData.rds')

entrybeh <- Baroni_UrbaniBuser_Function(BinaryData,1,11)
timebeh <- Baroni_UrbaniBuser_Function(BinaryData,12,21)
Pairwise <- Pairwise(BinaryData,5,entrybeh,timebeh)

RandomForest_SMOTE_balanced <- SMOTE(Pairwise,3,500)

sample <- sample(c(TRUE, FALSE), nrow(RandomForest_SMOTE_balanced ), replace=TRUE, prob=c(0.8,0.2))
train  <- RandomForest_SMOTE_balanced[sample, ]
test   <- RandomForest_SMOTE_balanced[!sample,]

rf <- randomForest(factor(Crime_Pairs)~.,train,importance=TRUE)

predict_rf <- predict(rf,test[,1:2])

accuracy <- confusionMatrix(predict_rf2, factor(test$Crime_Pairs))$overall
accuracy <- accuracy[1]

importance(rf)

rf$forest$xbestsplit <- round(rf$forest$xbestsplit,4)
for (i in round(runif(4,1,500))){
  reprtree::plot.getTree(rforest=rf,k=i,main=paste('Tree',i))
}
