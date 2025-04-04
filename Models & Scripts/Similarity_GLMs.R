library(flux)

BinaryData <- readRDS('Characteristics_BinaryData.rds')


#Similarity index
JacCoef_allEntryBeh <- JaccardCoef_Function(BinaryData,1,11)
JacCoef_Time <- JaccardCoef_Function(BinaryData,12,21)


CosineCoefEntry <- Cosine_Function(BinaryData,1,11)
CosineCoefTime <- Cosine_Function(BinaryData,12,21)

DiceCoefEntry <- Dice_Function(BinaryData,1,11)
DiceCoefTime <- Dice_Function(BinaryData,12,21)

BraunBanCoefEntry <- BraunBan_Function(BinaryData,1,11)
BraunBanCoefTime <- BraunBan_Function(BinaryData,12,21)

BUCoefEntry <- Baroni_UrbaniBuser_Function(BinaryData,1,11)
BUCoefTime <- Baroni_UrbaniBuser_Function(BinaryData,12,21)

YulesCoefEntry <- YulesQ_Function(BinaryData,1,11)
YulesCoefTime <- YulesQ_Function(BinaryData,12,21)

MichaelCoefEntry <- Micheal_Function(BinaryData,1,11)
MichaelCoefTime <- Micheal_Function(BinaryData,12,21)

#Pairwise Tables
Jaccard_Pairwise <- Pairwise(BinaryData,5,JacCoef_allEntryBeh,JacCoef_Time)
Cosine_pairwise <- Pairwise(BinaryData,5,CosineCoefEntry,CosineCoefTime)
Dice_Pairwise <- Pairwise(BinaryData,5,DiceCoefEntry,DiceCoefTime)
Michael_Pairwise <- Pairwise(BinaryData,5,MichaelCoefEntry,MichaelCoefTime)
BraunBan_Pairwise <- Pairwise(BinaryData,5,BraunBanCoefEntry,BraunBanCoefTime)
BUB_Pairwise <- Pairwise(BinaryData,5,BUCoefEntry,BUCoefTime)
Yules_Pairwise <- Pairwise(BinaryData,5,YulesCoefEntry,YulesCoefEntry)


#Trained Model

Jaccard_Test_Train <- glm_test_train(Jaccard_Pairwise)
Cosine_test_train <- glm_test_train(Cosine_pairwise)
Yules_test_train <- glm_test_train(Yules_Pairwise)
Dice_test_train <- glm_test_train(Dice_Pairwise)
BU_test_train <- glm_test_train(BUB_Pairwise)
Michael_test_train <- glm_test_train(Michael_Pairwise)
BraunBan_test_train <- glm_test_train(BraunBan_Pairwise)



# ROC Curve and AUC Value

Jaccard_AUC <- roc_curve(Jaccard_Test_Train,Jaccard_Pairwise)
Cosine_Auc <- roc_curve(Cosine_test_train,Cosine_pairwise)
Yule_AUC <- roc_curve(Yules_test_train,Yules_Pairwise)
Dice_Auc <- roc_curve(Dice_test_train,Dice_Pairwise) 
BUB_AUC <- roc_curve(BU_test_train,BUB_Pairwise)
Michael_AUC <- roc_curve(Michael_test_train,Michael_Pairwise)
BraunBan_AUC <- roc_curve(BraunBan_test_train,BraunBan_Pairwise)

SimilarityIndex <- rbind(Jaccard_AUC,Cosine_Auc,Yule_AUC,Dice_Auc,BUB_AUC,Michael_AUC,BraunBan_AUC)
SimilarityIndex <- data.frame(SimilarityIndex)

## must sens_vs_spec first
roc_curve_v2 <- function(predictions,df){
  table4 <- vector()
  coordinates<- data.frame()
  for (i in seq(from=0, to=10, by=1)){
    i <- i/10
    table4 <- rbind(table4,sens_vs_spec(i,predictions,df))
  }
  table4<- data.frame(table4)
  colnames(table4)<- c('sensitivity','specificity')
  x<- (1-table4$`specificity`)
  y <- table4$`sensitivity`
  coordinates <- cbind(x,y)
  similarity_name <- strsplit(deparse(substitute(df)), "_")[[1]]
  colnames(coordinates)<- c(paste(similarity_name[1],'x'),paste(similarity_name[1],'y'))
  return(coordinates)
}


Jaccard_coords <- roc_curve_v2(Jaccard_Test_Train,Jaccard_Pairwise)
Cosine_coords <- roc_curve_v2(Cosine_test_train,Cosine_pairwise)
Yule_coords <- roc_curve_v2(Yules_test_train,Yules_Pairwise)
Dice_coords <- roc_curve_v2(Dice_test_train,Dice_Pairwise) 
BUB_coords <- roc_curve_v2(BU_test_train,BUB_Pairwise)
Michael_coords <- roc_curve_v2(Michael_test_train,Michael_Pairwise)
BraunBan_coords <- roc_curve_v2(BraunBan_test_train,BraunBan_Pairwise)

library(RColorBrewer)
col <- brewer.pal(7, "Paired")

plot(Jaccard_coords, xlim=c(0,1),ylim = c(0,1), type = 'l', col = col[1] , lwd =2,
     xlab= 'False Positive Rate',ylab = 'True Positive Rate', main = paste('ROC Curve of Each GLM Model'))
abline(0,1)
lines(Cosine_coords,lwd =2, col = col[2])
lines(Yule_coords,lwd =2, col = col[3])
lines(Dice_coords,lwd =2, col =col[4])
lines(BUB_coords,lwd =2,col =col[5])
lines(Michael_coords,lwd =2, col=col[6])
lines(BraunBan_coords,lwd =2, col =col[7])
legend(x= 'bottomright', legend=c('Jaccard','Cosine',"Yule's Q",'Dice','Baroni Buster',
                               'Michael', 'Braun Blanquet'),fill = c(col), cex =0.55)