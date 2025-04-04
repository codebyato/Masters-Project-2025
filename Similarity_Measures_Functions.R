#Jaccard Similarity Function
JaccardCoef_Function <- function(df,start_col, end_col){
  
  CoefVec <- vector()
  crime_pair <- vector()

for (i in 1:(nrow(df)-1)){
  for (j in (i+1):nrow(df)){

    crime_i<- as.numeric(df[i,(start_col:end_col)])
    crime_j<- as.numeric(df[j,(start_col:end_col)])
    
    ij_intersect <-0
    ij_union <-0
    for (x in 1:length(crime_i)){
      if (crime_i[x] == crime_j[x] & crime_i[x] ==1){
        ij_intersect <- ij_intersect + 1
      }
      if (crime_i[x] ==1 | crime_j[x] == 1){
        ij_union <- ij_union +1
      }
    }
    if (ij_union >0){
      Coef <- (ij_intersect/ij_union)
      CoefVec <- c(CoefVec, Coef)
      crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
    }
    if (ij_union ==0 ){
      JacCoef <- 0
      CoefVec <- c(CoefVec, Coef)
      crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
    }
  }
}
  outputCoef <- matrix(CoefVec)
  rownames(outputCoef) <- crime_pair
  colnames(outputCoef) <- c('Jaccard Coef')
  return(outputCoef)
}

# Cosine Function 
Cosine_Function <- function(df,start_col, end_col){
  
  CoefVec <- vector()
  crime_pair <- vector()
  
  for (i in 1:(nrow(df)-1)){
    for (j in (i+1):nrow(df)){
      
      crime_i<- as.numeric(df[i,(start_col:end_col)])
      crime_j<- as.numeric(df[j,(start_col:end_col)])
      
      ij_11 <-0
      ij_10 <-0
      ij_01 <- 0
      ij_00 <- 0
      for (x in 1:length(crime_i)){
        if (crime_i[x] == crime_j[x] & crime_i[x] ==1){
          ij_11 <- ij_11 + 1
        }
        if (crime_i[x] ==1 & crime_j[x] == 0){
          ij_10 <- ij_10 +1
        }
        if (crime_i[x] ==0 & crime_j[x] == 1){
          ij_01 <- ij_01 +1
        }
        if (crime_i[x] == crime_j[x] & crime_i[x] ==0){
          ij_00 <- ij_00 +1
        }
      }
      topfrac <- ij_11
      botfrac <- sqrt((ij_11 +ij_10)+(ij_11 +ij_01))
      if (botfrac >0){
        Coef <- (topfrac/botfrac)
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
      if (botfrac ==0 ){
        Coef <- 0
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
    }
  }
  outputCoef <- matrix(CoefVec)
  rownames(outputCoef) <- crime_pair
  colnames(outputCoef) <- c('Cosine Coef')
  return(outputCoef)
}

# Dice Function 
Dice_Function <- function(df,start_col, end_col){
  CoefVec <- vector()
  crime_pair <- vector()
  for (i in 1:(nrow(df)-1)){
    for (j in (i+1):nrow(df)){
      
      crime_i<- as.numeric(df[i,(start_col:end_col)])
      crime_j<- as.numeric(df[j,(start_col:end_col)])
      
      ij_11 <-0
      ij_10 <-0
      ij_01 <- 0
      ij_00 <- 0
      for (x in 1:length(crime_i)){
        if (crime_i[x] == crime_j[x] & crime_i[x] ==1){
          ij_11 <- ij_11 + 1
        }
        if (crime_i[x] ==1 & crime_j[x] == 0){
          ij_10 <- ij_10 +1
        }
        if (crime_i[x] ==0 & crime_j[x] == 1){
          ij_01 <- ij_01 +1
        }
        if (crime_i[x] == crime_j[x] & crime_i[x] ==0){
          ij_00 <- ij_00 +1
        }
      }
      topfrac <- (2* ij_11)
      botfrac <- ((2*ij_11) +ij_10 + ij_01)
      if (botfrac >0){
        Coef <- (topfrac/botfrac)
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
      if (botfrac ==0 ){
        Coef <- 0
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
    }
  }
  outputCoef <- matrix(CoefVec)
  rownames(outputCoef) <- crime_pair
  colnames(outputCoef) <- c('Dice Coef')
  return(outputCoef)
}

# Michael Function
Micheal_Function <- function(df,start_col, end_col){
  CoefVec <- vector()
  crime_pair <- vector()
  for (i in 1:(nrow(df)-1)){
    for (j in (i+1):nrow(df)){
      
      crime_i<- as.numeric(df[i,(start_col:end_col)])
      crime_j<- as.numeric(df[j,(start_col:end_col)])
      
      ij_11 <-0
      ij_10 <-0
      ij_01 <- 0
      ij_00 <- 0
      for (x in 1:length(crime_i)){
        if (crime_i[x] == crime_j[x] & crime_i[x] ==1){
          ij_11 <- ij_11 + 1
        }
        if (crime_i[x] ==1 & crime_j[x] == 0){
          ij_10 <- ij_10 +1
        }
        if (crime_i[x] ==0 & crime_j[x] == 1){
          ij_01 <- ij_01 +1
        }
        if (crime_i[x] == crime_j[x] & crime_i[x] ==0){
          ij_00 <- ij_00 +1
        }
      }
      topfrac <- (4*((ij_11*ij_00) - (ij_01*ij_01)))
      botfrac <- ((ij_11*ij_00)^2 + (ij_01*ij_01)^2)
      if (botfrac >0){
        Coef <- (topfrac/botfrac)
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
      if (botfrac ==0 ){
        Coef <- 0
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
    }
  }
  outputCoef <- matrix(CoefVec)
  rownames(outputCoef) <- crime_pair
  colnames(outputCoef) <- c('Michael Coef')
  return(outputCoef)
}

#Braun Blanquet Function
BraunBan_Function <- function(df,start_col, end_col){
  CoefVec <- vector()
  crime_pair <- vector()
  
    for (i in 1:(nrow(df)-1)){
      for (j in (i+1):nrow(df)){
        
        crime_i<- as.numeric(df[i,(start_col:end_col)])
        crime_j<- as.numeric(df[j,(start_col:end_col)])
        
        ij_11 <-0
        ij_10 <-0
        ij_01 <- 0
        ij_00 <- 0
        for (x in 1:length(crime_i)){
          if (crime_i[x] == crime_j[x] & crime_i[x] ==1){
            ij_11 <- ij_11 + 1
          }
          if (crime_i[x] ==1 & crime_j[x] == 0){
            ij_10 <- ij_10 +1
          }
          if (crime_i[x] ==0 & crime_j[x] == 1){
            ij_01 <- ij_01 +1
          }
          if (crime_i[x] == crime_j[x] & crime_i[x] ==0){
            ij_00 <- ij_00 +1
          }
        }
        ac <- ij_11 +ij_10
        ab <- ij_11 + ij_01
        botfrac <- max(ac,ab)
        if (botfrac >0){
          Coef <- (ij_11/botfrac)
          CoefVec <- c(CoefVec, Coef)
          crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
        }
        if (botfrac ==0 ){
          Coef <- 0
          CoefVec <- c(CoefVec, Coef)
          crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
        }
      }
    }
  outputCoef <- matrix(CoefVec)
  rownames(outputCoef) <- crime_pair
  colnames(outputCoef) <- c('Braun Blanquet Coef')
  return(outputCoef)
}

#Baroni-Urbani Buser Function 
Baroni_UrbaniBuser_Function <- function(df,start_col, end_col){
  CoefVec <- vector()
  crime_pair <- vector()
  for (i in 1:(nrow(df)-1)){
    for (j in (i+1):nrow(df)){
      
      crime_i<- as.numeric(df[i,(start_col:end_col)])
      crime_j<- as.numeric(df[j,(start_col:end_col)])
      
      ij_11 <-0
      ij_10 <-0
      ij_01 <- 0
      ij_00 <- 0
      for (x in 1:length(crime_i)){
        if (crime_i[x] == crime_j[x] & crime_i[x] ==1){
          ij_11 <- ij_11 + 1
        }
        if (crime_i[x] ==1 & crime_j[x] == 0){
          ij_10 <- ij_10 +1
        }
        if (crime_i[x] ==0 & crime_j[x] == 1){
          ij_01 <- ij_01 +1
        }
        if (crime_i[x] == crime_j[x] & crime_i[x] ==0){
          ij_00 <- ij_00 +1
        }
      }
     ad <- ij_11*ij_00
      topfrac <- sqrt(ad) +ij_11
      botfrac <- sqrt(ad) +ij_11 +ij_10 +ij_01 
      if (botfrac >0){
        Coef <- (topfrac/botfrac)
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
      if (botfrac ==0 ){
        Coef <- 0
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
    }
  }
  outputCoef <- matrix(CoefVec)
  rownames(outputCoef) <- crime_pair
  colnames(outputCoef) <- c('BUB Coef')
  return(outputCoef)
}

#Yule's Q Function 
YulesQ_Function <- function(df,start_col, end_col){
  CoefVec <- vector()
  crime_pair <- vector()
  for (i in 1:(nrow(df)-1)){
    for (j in (i+1):nrow(df)){
      
      crime_i<- as.numeric(df[i,(start_col:end_col)])
      crime_j<- as.numeric(df[j,(start_col:end_col)])
      
      ij_11 <-0
      ij_10 <-0
      ij_01 <- 0
      ij_00 <- 0
      for (x in 1:length(crime_i)){
        if (crime_i[x] == crime_j[x] & crime_i[x] ==1){
          ij_11 <- ij_11 + 1
        }
        if (crime_i[x] ==1 & crime_j[x] == 0){
          ij_10 <- ij_10 +1
        }
        if (crime_i[x] ==0 & crime_j[x] == 1){
          ij_01 <- ij_01 +1
        }
        if (crime_i[x] == crime_j[x] & crime_i[x] ==0){
          ij_00 <- ij_00 +1
        }
      }
      topfrac <- (ij_11*ij_00) - (ij_10*ij_01)
      botfrac <- (ij_11*ij_00) + (ij_10*ij_01)
      if (botfrac >0){
        Coef <- (topfrac/botfrac)
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
      if (botfrac ==0 ){
        Coef <- 0
        CoefVec <- c(CoefVec, Coef)
        crime_pair <- c(crime_pair, paste0("C", i, " / C", j))
      }
    }
  }
  outputCoef <- matrix(CoefVec)
  rownames(outputCoef) <- crime_pair
  colnames(outputCoef) <- c("Yule's Q Coef")
  return(outputCoef)
}
