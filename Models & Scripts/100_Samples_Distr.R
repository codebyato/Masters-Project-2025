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




#Test and Train
auc_distribution <- function(Pairwise_df){
  Coef_AUC <- vector()
  linked <- Pairwise_df[,ncol(Pairwise_df),drop=FALSE]
  for (i in 1:100){
    set.seed(i)
    test_train <- function(df){
      sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
      train  <- df[sample, ]
      test   <- df[!sample,c('Coef_Entry_Behaviour', 'Coef_Time')]
      
      model2 <- glm(Crime_Pairs ~ Coef_Entry_Behaviour + Coef_Time, data = train, family= binomial(link = "logit"))
      summary(model2)
      predictions <- data.frame(probs = predict(model2, newdata = test, type = "response"))
      return(predictions)
    }
    sens_vs_spec <- function(x,vector){
      predict_real <- vector()
      
      for (j in 1:nrow(vector)) {
        
        if (vector[j,] >= x ) {
          predict_real[j] <- 1
        } else if (vector[j,] < x) {
          predict_real[j] <- 0
        }
      }
      table2 <- cbind(vector,predict_real)
      
      table3 <- merge(table2,linked, by = 'row.names')
      
      tp <- 0
      tn <- 0
      fn <- 0
      fp <- 0
      
      for (i in 1:nrow(table3)){
        if (table3[i,3] == table3[i,4] & table3[i,4]== 1){
          tp <- tp +1
        }
        if (table3[i,3] == table3[i,4] & table3[i,4] == 0){
          tn <- tn +1
        }
        if (table3[i,3] ==0 & table3[i,4] == 1){
          fn <- fn +1
        }
        if (table3[i,3] ==1 & table3[i,4] == 0){
          fp <- fp +1
        }
      }
      
      sensitivity <- tp /(tp +fn)
      specificity <- tn /(tn + fp)
      sen_spec <- c(sensitivity,specificity)
      
      return(sen_spec)
    }
    
    #ROC & AUC
    roc_curve <- function(predictions){
      table4 <- vector()
      for (i in seq(from=0, to=10, by=1)){
        i <- i/10
        table4 <- rbind(table4,sens_vs_spec(i,predictions))
      }
      table4<- data.frame(table4)
      colnames(table4)<- c('sensitivity','specificity')
      x<- (1-table4$`specificity`)
      y <- table4$`sensitivity`
      AUC <- auc(x,y)
      return(AUC)
    }
    
    Coef_Test_Train <- test_train(Pairwise_df)
    Coef_AUC <- c(Coef_AUC,roc_curve(Coef_Test_Train))
  }
  return(Coef_AUC)
}

