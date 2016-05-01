setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory

fileLoads <- c("AdaClassBase", "AdaClass3", "AdaClass4", "AdaClass5", "AdaClass6",
               "AdaRegBase", "AdaReg3", "AdaReg4", "AdaReg5", "AdaReg6",
               "BayesianRidgeBase",
               "ElasticNetBase",
               "ETC10_8", "ETC20_15", "ETC30_23", "ETC40_30", "ETC50_37", "ETC60_45",
               "ETR10_8", "ETR20_15", "ETR30_23",
               "GaussianNBBase",
               "knn2", "knn4", "knn8", "knn16", "knn32", "knn64", "knn128", "knn256",
               "LDABase",
               "LeastAngleBase",
               "LinRegBase",
               "LogRegBase",
               "NearCentroidBase",
               "PasAggC1", "PasAggC2", "PasAggR1", "PasAggR2",
               "RFC10_8", "RFC20_15", "RFC30_23", "RFC40_30", "RFC50_37", "RFC60_45",
               "RFR10_8", "RFR20_15", "RFR30_23", "RFR40_30", "RFR50_37", "RFR60_45",
               "RidgeCVBase",
               "XGBrank", "XGBbinary")

train_prob <- data.frame(del = rep(0, 76020))
test_prob <- data.frame(del = rep(0, 75818))
for (i in fileLoads) {
  cat("Adding ", i, "...", sep = "")
  train_prob <- cbind(train_prob, delN = read.csv(paste("Stacker2/train_11111_", i, "_1.csv", sep = ""), header = FALSE)$V1)
  colnames(train_prob)[NROW(colnames(train_prob))] <- paste(i, "_1", sep = "")
  test_prob <- cbind(test_prob, delN = read.csv(paste("Stacker2/test_11111_", i, "_1.csv", sep = ""), header = FALSE)$V1)
  colnames(test_prob)[NROW(colnames(test_prob))] <- paste(i, "_1", sep = "")
  #tempROC <- FastROC(target, train_prob[, NROW(colnames(train_prob))])
}
train_prob <- train_prob[, -1]
test_prob <- test_prob[, -1]

train_prob <- train_prob[, !(names(train_prob) %in% c("PasAggR1_1", "PasAggC2_1"))]
test_prob <- test_prob[, !(names(test_prob) %in% c("PasAggR1_1", "PasAggC2_1"))]

write.csv(train_prob, file = "train_prob_lv2.csv", row.names = FALSE)
write.csv(test_prob, file = "test_prob_lv2.csv", row.names = FALSE)
