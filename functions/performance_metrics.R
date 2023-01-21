################################################
# CUstom function for model performance metrics

model_perf_metrics <- function(actual, pred){
  TP <- sum(as.integer(actual == 1)&(pred == 1))
  TN <- sum(as.integer(actual == 0)&(pred == 0))
  
  FP <- sum(as.integer(actual == 0)&(pred == 1))
  FN <- sum(as.integer(actual == 1)&(pred == 0))

  accuracy <- mean(as.integer(actual == pred))
  
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  
  f1_score <- (2 * precision * recall)/(precision + recall)
  
  list(accuracy = accuracy
       ,precision = precision
       ,recall = recall
       ,f1_score = f1_score)
  }