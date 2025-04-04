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

SVM_Model <- function (df,entry_start,entry_end,time_start,time_end,
                       linkedpairs,oversample){
  entrybeh <- Baroni_UrbaniBuser_Function(df,entry_start,entry_end)
  timebeh <- Baroni_UrbaniBuser_Function(df,time_start,time_end)
  PairwiseTable <- Pairwise(df,linkedpairs,entrybeh,timebeh)
  
  if ( oversample == "SMOTE" ) {
    Balanced_Pairwise <- SMOTE(PairwiseTable,5 ,500)
  }
  else if ( oversample == "ROSE" ) {
    library(ROSE)
    curr_frame <<- sys.nframe()
    Balanced_Pairwise <- ovun.sample(Crime_Pairs~. , data =get("PairwiseTable", 
                                                        sys.frame(curr_frame)), 
                                                        method = "over")$data
  }
  Balanced_Pairwise$Crime_Pairs <-
    factor(Balanced_Pairwise$Crime_Pairs )
  sample <- sample ( c ( TRUE , FALSE ) , nrow ( Balanced_Pairwise ) ,
                     replace = TRUE , prob = c (0.8 ,02) )
  train <- Balanced_Pairwise [ sample ,]
  test <- Balanced_Pairwise [ ! sample ,]
  model <- svm( Crime_Pairs ~. , data = train , kernel = "radial" ,
                 probability = TRUE )
  pred <- predict(model , test , type = "prob" , probability = TRUE )
  pred <- attr( pred , "probabilities" )
  pred <- as.data.frame(pred[ , c("1") , drop = FALSE ])
  SVM_AUC <- roc_curve( pred, Balanced_Pairwise)
  return(SVM_AUC)
}

smote_auc <- data.frame()

smote_auc <- rbind(smote_auc,SVM_Model(OriginalBinaryData,1,11,12,21,5,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_A,1,11,12,21,10,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_B,1,11,12,21,5,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_C,1,11,12,21,10,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_D,1,11,12,22,5,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_E,1,13,14,23,5,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_F,1,18,19,28,5,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_G,1,20,21,31,5,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_H,1,20,21,31,10,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_I,1,20,21,31,5,'SMOTE'))
smote_auc <- rbind(smote_auc,SVM_Model(Dataset_J,1,20,21,31,10,'SMOTE'))

ROSE_auc <- data.frame()

ROSE_auc <- rbind(ROSE_auc,SVM_Model(OriginalBinaryData,1,11,12,21,5,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_A,1,11,12,21,10,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_B,1,11,12,21,5,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_C,1,11,12,21,10,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_D,1,11,12,22,5,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_E,1,13,14,23,5,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_F,1,18,19,28,5,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_G,1,20,21,31,5,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_H,1,20,21,31,10,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_I,1,20,21,31,5,'ROSE'))
ROSE_auc <- rbind(ROSE_auc,SVM_Model(Dataset_J,1,20,21,31,10,'ROSE'))



comparison <- cbind(smote_auc,ROSE_auc)

colnames(comparison) <- c('SMOTE Oversampled Data AUC', 
                          'ROSE Oversampled Data AUC')

plot(comparison,pch=16, main = 'SMOTE Oversampled Data vs ROSE Oversampled Data SVM AUC Values', 
     cex.main = 0.8)
abline(0,1)

