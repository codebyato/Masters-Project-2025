library(e1071)

OriginalBinaryData <- readRDS('Characteristics_BinaryData.rds')
Dataset_A <- readRDS('Dataset_A.rds')
Dataset_B <- readRDS('Dataset_B.rds')
Dataset_C <- readRDS('Dataset_C.rds')
Dataset_D <- readRDS('Dataset_D.rds')
Dataset_E <- readRDS('Dataset_E.rds')
Dataset_F <- readRDS('Dataset_F.rds')
Dataset_G <- readRDS('Dataset_G.rds')
Dataset_H <- readRDS('Dataset_H.rds')
Dataset_I <- readRDS('Dataset_I.rds')
Dataset_J <- readRDS('Dataset_J.rds')


SVM_GLM_Comparison <- function(df, entry_start, entry_end,time_start, time_end,linkedpairs,oversample){
  
  entrybeh <- Baroni_UrbaniBuser_Function(df,entry_start,entry_end)
  timebeh <- Baroni_UrbaniBuser_Function(df,time_start,time_end)
  
  PairwiseTable <- Pairwise(df,linkedpairs,entrybeh,timebeh)
  
  if (oversample == 'SMOTE'){
    Balanced_Pairwise <- SMOTE(PairwiseTable,5,500)
  }
  else if (oversample == 'ROSE'){
    library(ROSE)
    curr_frame <<- sys.nframe()
    Balanced_Pairwise <- ovun.sample(Crime_Pairs~., data = get('PairwiseTable',sys.frame(curr_frame)), method='over')$data
  }
  
  
  Balanced_Pairwise$Crime_Pairs <- factor(Balanced_Pairwise$Crime_Pairs)
  set.seed(1)
  sample <- sample(c(TRUE,FALSE), nrow(Balanced_Pairwise), replace = TRUE, prob = c(0.8,02))
  train <- Balanced_Pairwise[sample,]
  test <- Balanced_Pairwise[!sample,]
  
  model <- svm(Crime_Pairs~., data = train, kernel = 'radial', probability = TRUE)
  pred <- predict(model,test, type ='prob', probability = TRUE)
  pred <- attr(pred,'probabilities')
  
  pred <- as.data.frame(pred[,c('1'), drop=FALSE])
  SVM_AUC <- roc_curve(pred,Balanced_Pairwise)
  
  glm_results <- glm_test_train(Balanced_Pairwise)
  glm_AUC <- roc_curve(glm_results,Balanced_Pairwise)

  results <- data.frame()
  results <- cbind(SVM_AUC,glm_AUC)
  colnames(results) <- c('SVM AUC','GLM AUC')
  rownames(results) <- deparse(substitute(df))
  
  return(results)
}

smote_comparison <- data.frame()

smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(OriginalBinaryData,
                                                              1,11,12,21,5,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_A,1,11,12,
                                                              21,10,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_B,1,11,12,
                                                              21,5,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_C,1,11,12,
                                                              21,10,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_D,1,11,12,
                                                              22,5,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_E,1,13,14,
                                                              23,5,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_F,1,18,19,
                                                              28,5,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_G,1,20,21,
                                                              31,5,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_H,1,20,21,
                                                              31,10,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_I,1,20,21,
                                                              31,5,'SMOTE'))
smote_comparison <- rbind(smote_comparison,SVM_GLM_Comparison(Dataset_J,1,20,21,
                                                              31,10,'SMOTE'))

ROSE_comparison <- data.frame()

ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(OriginalBinaryData,1
                                                            ,11,12,21,5,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_A,1,11,12,21
                                                            ,10,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_B,1,11,12,21
                                                            ,5,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_C,1,11,12,21
                                                            ,10,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_D,1,11,12,22
                                                            ,5,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_E,1,13,14,23
                                                            ,5,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_F,1,18,19,28,5
                                                            ,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_G,1,20,21,31,5
                                                            ,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_H,1,20,21,31,10
                                                            ,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_I,1,20,21,31,5
                                                            ,'ROSE'))
ROSE_comparison <- rbind(ROSE_comparison,SVM_GLM_Comparison(Dataset_J,1,20,21,31,10
                                                            ,'ROSE'))


plot(ROSE_comparison,col='black',pch =16, main = 'SVM AUC Value vs GLM AUC Value')
points(smote_comparison,col='blue', pch = 16)
abline(0,1)
legend(x = 'topleft', legend = c('SMOTE Oversampled Data', 'ROSE Oversampled Data'),
       fill = c('blue','black'), cex =0.7)

glm_svm <- cbind(smote_comparison,ROSE_comparison)
colnames(glm_svm)<- c('SMOTE SVM AUC', 'SMOTE GLM AUC', 'ROSE SVM AUC', 'ROSE GLM AUC')


