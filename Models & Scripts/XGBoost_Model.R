library(xgboost)
library(caret)

BinaryData <- readRDS('Characteristics_BinaryData.rds')

entrybeh <- Baroni_UrbaniBuser_Function(BinaryData,1,11)
timebeh <- Baroni_UrbaniBuser_Function(BinaryData,12,21)
Pairwise <- Pairwise(BinaryData,5,entrybeh,timebeh)


XGBoost_SMOTE_balanced <- SMOTE(Pairwise,3,500)
XGBoost_SMOTE_balanced <- data.matrix(XGBoost_SMOTE_balanced, rownames = TRUE)

sample <- sample(c(TRUE, FALSE), nrow(XGBoost_SMOTE_balanced ), replace=TRUE, prob=c(0.8,0.2))
train  <-XGBoost_SMOTE_balanced[sample, ]
test   <-XGBoost_SMOTE_balanced[!sample,]

xgb <-xgboost(data = train[,1:2], label = train[,3], nrounds = 6,
              objective = "binary:hinge")


results <- predict(xgb,test[,1:2])


accuracy <- confusionMatrix(factor(results), factor(test[,c('Crime_Pairs')]))$overall
accuracy <- accuracy[1]


library(DiagrammeR)
xgb.plot.tree(
  feature_names = c("Coef_Entry_Behaviour", "Coef_Time"),
  model = xgb,
  trees = 0:5)
