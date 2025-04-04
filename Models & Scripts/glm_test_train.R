glm_test_train <- function(df){
  set.seed(1)
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
  train  <- df[sample, ]
  test   <- df[!sample,c('Coef_Entry_Behaviour', 'Coef_Time')]
  
  model2 <- glm(Crime_Pairs ~ Coef_Entry_Behaviour + Coef_Time, data = train, family= binomial(link = "logit"))
  predictions <- data.frame(probs = predict(model2, newdata = test, type = "response"))
  return(predictions)
}
