library(ROSE)
BinaryData <- readRDS('Characteristics_BinaryData.rds')

BUCoefEntry <- Baroni_UrbaniBuser_Function(BinaryData,1,11)
BUCoefTime <- Baroni_UrbaniBuser_Function(BinaryData,12,21)

PairwiseTable <- Pairwise(BinaryData,5,BUCoefEntry,BUCoefTime)

smote_pairwise <- SMOTE(PairwiseTable,2,500)
adasyn_pairwise <- ADASYN_function(PairwiseTable,2,1)
rose_pairwise <- ovun.sample(Crime_Pairs~.,data = PairwiseTable, 
                             method = 'over')$data


overfitting_test <- function(df){
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
  train  <- df[sample, ]
  test   <- df[!sample,1:2]
  
  GLM <- glm(Crime_Pairs ~., data = train, family= binomial(link = "logit"))
  
  test_predictions <- data.frame(probs = predict(GLM, newdata = test, 
                                                 type = "response"))
  train_predictions <- data.frame(probs = predict(GLM, newdata = train,
                                                  type = "response"))
  
  testpart <- roc_curve(test_predictions,df)
  trainpart <- roc_curve(train_predictions,df)
  
  print(paste('Test AUC is',testpart))
  print(paste('Train AUC is',trainpart))
}

overfitting_test(rose_pairwise)
overfitting_test(smote_pairwise)
overfitting_test(adasyn_pairwise)
