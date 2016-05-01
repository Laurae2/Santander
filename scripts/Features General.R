remove(list = ls())
setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
train <- read.csv("train.csv")
test  <- read.csv("test.csv")




# Generate Set 1
# 307 features, 0.842 Public LB

train_temp <- train
test_temp <- test

# removing ID
train_temp$ID <- NULL
test_temp$ID <- NULL

# extracting label
train_target <- train$TARGET
train_temp$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train_temp$n0 <- apply(train_temp, 1, FUN=count0)
test_temp$n0 <- apply(test_temp, 1, FUN=count0)

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train_temp)) {
  if (length(unique(train_temp[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train_temp[[f]] <- NULL
    test_temp[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train_temp), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if ((all(train_temp[[f1]] == train_temp[[f2]]))) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train_temp), toRemove)

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]

#train_temp$TARGET <- train_target

write.csv(train_temp, file = "train_set1.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set1.csv", row.names = FALSE)




# Generate Set 2
# 10 features, 0.837 LB
# Added n0, removed std

train_temp <- train
test_temp <- test

# removing ID
train_temp$ID <- NULL
test_temp$ID <- NULL

# extracting label
train_target <- train$TARGET
train_temp$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train_temp$n0 <- apply(train_temp, 1, FUN=count0)
test_temp$n0 <- apply(test_temp, 1, FUN=count0)

feature.names <- c("var15", "saldo_var30", "n0", "num_var22_ult3", "imp_op_var39_ult1", "num_var45_hace3", "saldo_medio_var5_hace2", "var3", "saldo_medio_var8_ult3", "ind_var41_0")

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]

#train_temp$TARGET <- train_target

write.csv(train_temp, file = "train_set2.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set2.csv", row.names = FALSE)



# Generate Set 3
# 62 features, 0.841 LB
# Removed PCA

train_temp <- train
test_temp <- test

# removing ID
train_temp$ID <- NULL
test_temp$ID <- NULL

# extracting label
train_target <- train$TARGET
train_temp$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train_temp$n0 <- apply(train_temp, 1, FUN=count0)
test_temp$n0 <- apply(test_temp, 1, FUN=count0)

feature.names <- c('num_var39_0',  # 0.00031104199066874026
                   'ind_var13',  # 0.00031104199066874026
                   'num_op_var41_comer_ult3',  # 0.00031104199066874026
                   'num_var43_recib_ult1',  # 0.00031104199066874026
                   'imp_op_var41_comer_ult3',  # 0.00031104199066874026
                   'num_var8',  # 0.00031104199066874026
                   'num_var42',  # 0.00031104199066874026
                   'num_var30',  # 0.00031104199066874026
                   'saldo_var8',  # 0.00031104199066874026
                   'num_op_var39_efect_ult3',  # 0.00031104199066874026
                   'num_op_var39_comer_ult3',  # 0.00031104199066874026
                   'num_var41_0',  # 0.0006220839813374805
                   'num_op_var39_ult3',  # 0.0006220839813374805
                   'saldo_var13',  # 0.0009331259720062209
                   'num_var30_0',  # 0.0009331259720062209
                   'ind_var37_cte',  # 0.0009331259720062209
                   'ind_var39_0',  # 0.001244167962674961
                   'num_var5',  # 0.0015552099533437014
                   'ind_var10_ult1',  # 0.0015552099533437014
                   'num_op_var39_hace2',  # 0.0018662519440124418
                   'num_var22_hace2',  # 0.0018662519440124418
                   'num_var35',  # 0.0018662519440124418
                   'ind_var30',  # 0.0018662519440124418
                   'num_med_var22_ult3',  # 0.002177293934681182
                   'imp_op_var41_efect_ult1',  # 0.002488335925349922
                   'var36',  # 0.0027993779160186624
                   'num_med_var45_ult3',  # 0.003110419906687403
                   'imp_op_var39_ult1',  # 0.0037325038880248835
                   'imp_op_var39_comer_ult3',  # 0.0037325038880248835
                   'imp_trans_var37_ult1',  # 0.004043545878693624
                   'num_var5_0',  # 0.004043545878693624
                   'num_var45_ult1',  # 0.004665629860031105
                   'ind_var41_0',  # 0.0052877138413685845
                   'imp_op_var41_ult1',  # 0.0052877138413685845
                   'num_var8_0',  # 0.005598755832037325
                   'imp_op_var41_efect_ult3',  # 0.007153965785381027
                   'num_op_var41_ult3',  # 0.007153965785381027
                   'num_var22_hace3',  # 0.008087091757387248
                   'num_var4',  # 0.008087091757387248
                   'imp_op_var39_comer_ult1',  # 0.008398133748055987
                   'num_var45_ult3',  # 0.008709175738724729
                   'ind_var5',  # 0.009953343701399688
                   'imp_op_var39_efect_ult3',  # 0.009953343701399688
                   'num_meses_var5_ult3',  # 0.009953343701399688
                   'saldo_var42',  # 0.01181959564541213
                   'imp_op_var39_efect_ult1',  # 0.013374805598755831
                   'num_var45_hace2',  # 0.014618973561430793
                   'num_var22_ult1',  # 0.017107309486780714
                   'saldo_medio_var5_ult1',  # 0.017418351477449457
                   'saldo_var5',  # 0.0208398133748056
                   'ind_var8_0',  # 0.021150855365474338
                   'ind_var5_0',  # 0.02177293934681182
                   'num_meses_var39_vig_ult3',  # 0.024572317262830483
                   'saldo_medio_var5_ult3',  # 0.024883359253499222
                   'num_var45_hace3',  # 0.026749611197511663
                   'num_var22_ult3',  # 0.03452566096423017
                   'saldo_medio_var5_hace3',  # 0.04074650077760498
                   'saldo_medio_var5_hace2',  # 0.04292379471228616
                   'n0',  # 0.04696734059097978
                   'saldo_var30',  # 0.09611197511664074
                   'var38',  # 0.1390357698289269
                   'var15')  # 0.20964230171073095

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]

#train_temp$TARGET <- train_target

write.csv(train_temp, file = "train_set3.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set3.csv", row.names = FALSE)



# Generate Set 4

setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
full_data <- as.data.frame(fread("fulldata - Copy2.csv", header = TRUE, sep = ","))
full_data_temp <- full_data[-151839, !(colnames(full_data) %in% c("SinglesNN15", "SinglesNN30", "SinglesNN38",
                                                                  "NzvNN15", "NzvNN30", "NzvNN38",
                                                                  "var5NN15", "var5NN30", "var5NN38",
                                                                  "var8NN15", "var8NN30", "var8NN38",
                                                                  "var12NN15", "var12NN30", "var12NN38",
                                                                  "var13NN15", "var13NN30", "var13NN38",
                                                                  "var30NN15", "var30NN30", "var30NN38",
                                                                  "var37NN15", "var37NN30", "var37NN38",
                                                                  "var39NN15", "var39NN30", "var39NN38",
                                                                  "var41NN15", "var41NN30", "var41NN38",
                                                                  "var42NN15", "var42NN30", "var42NN38",
                                                                  "var43NN15", "var43NN30", "var43NN38",
                                                                  "var45NN15", "var45NN30", "var45NN38"))]

train_temp <- full_data_temp[1:76020, ]
test_temp <- full_data_temp[76021:151838, ]

write.csv(train_temp, file = "train_set4.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set4.csv", row.names = FALSE)

write.csv(train_target, file = "train_target.csv", row.names = FALSE, col.names = FALSE)







# EXTRA DATA ~~~~~~~~~~~


# create base data

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
  train_prob <- cbind(train_prob, delN = read.csv(paste("Stacker2/train_11111_", i, "_1.csv", sep = ""), header = FALSE)$V1)
  colnames(train_prob)[NROW(colnames(train_prob))] <- paste(i, "_1", sep = "")
  test_prob <- cbind(test_prob, delN = read.csv(paste("Stacker2/test_11111_", i, "_1.csv", sep = ""), header = FALSE)$V1)
  colnames(test_prob)[NROW(colnames(test_prob))] <- paste(i, "_1", sep = "")
  tempROC <- FastROC(target, train_prob[, NROW(colnames(train_prob))])
}
train_prob <- train_prob[, -1]
test_prob <- test_prob[, -1]
full_prob <- rbind(train_prob, test_prob)
decor <- findCorrelation(cor(full_prob), cutoff = .99, verbose = FALSE, names = FALSE)
train_prob <- train_prob[, -decor]
test_prob <- test_prob[, -decor]



# Generate Enhanced Set 1
# 307 features

train_temp <- train
test_temp <- test

# removing ID
train_temp$ID <- NULL
test_temp$ID <- NULL

# extracting label
train_target <- train$TARGET
train_temp$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train_temp$n0 <- apply(train_temp, 1, FUN=count0)
test_temp$n0 <- apply(test_temp, 1, FUN=count0)

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train_temp)) {
  if (length(unique(train_temp[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train_temp[[f]] <- NULL
    test_temp[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train_temp), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if ((all(train_temp[[f1]] == train_temp[[f2]]))) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train_temp), toRemove)

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]
train_temp <- cbind(train_temp, train_prob)
test_temp <- cbind(test_temp, test_prob)

#train_temp$TARGET <- train_target

write.csv(train_temp, file = "train_set1_Enhanced.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set1_Enhanced.csv", row.names = FALSE)



# Generate Set 4

setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
full_data <- as.data.frame(fread("fulldata - Copy2.csv", header = TRUE, sep = ","))
full_data_temp <- full_data[-151839, !(colnames(full_data) %in% c("SinglesNN15", "SinglesNN30", "SinglesNN38",
                                                                  "NzvNN15", "NzvNN30", "NzvNN38",
                                                                  "var5NN15", "var5NN30", "var5NN38",
                                                                  "var8NN15", "var8NN30", "var8NN38",
                                                                  "var12NN15", "var12NN30", "var12NN38",
                                                                  "var13NN15", "var13NN30", "var13NN38",
                                                                  "var30NN15", "var30NN30", "var30NN38",
                                                                  "var37NN15", "var37NN30", "var37NN38",
                                                                  "var39NN15", "var39NN30", "var39NN38",
                                                                  "var41NN15", "var41NN30", "var41NN38",
                                                                  "var42NN15", "var42NN30", "var42NN38",
                                                                  "var43NN15", "var43NN30", "var43NN38",
                                                                  "var45NN15", "var45NN30", "var45NN38"))]

train_temp <- full_data_temp[1:76020, ]
test_temp <- full_data_temp[76021:151838, ]

write.csv(train_temp, file = "train_set4.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set4.csv", row.names = FALSE)




# Generate Set 4 Enhanced

setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
full_data <- as.data.frame(fread("fulldata - Copy2.csv", header = TRUE, sep = ","))
full_data_temp <- full_data[-151839, !(colnames(full_data) %in% c("SinglesNN15", "SinglesNN30", "SinglesNN38",
                                                                  "NzvNN15", "NzvNN30", "NzvNN38",
                                                                  "var5NN15", "var5NN30", "var5NN38",
                                                                  "var8NN15", "var8NN30", "var8NN38",
                                                                  "var12NN15", "var12NN30", "var12NN38",
                                                                  "var13NN15", "var13NN30", "var13NN38",
                                                                  "var30NN15", "var30NN30", "var30NN38",
                                                                  "var37NN15", "var37NN30", "var37NN38",
                                                                  "var39NN15", "var39NN30", "var39NN38",
                                                                  "var41NN15", "var41NN30", "var41NN38",
                                                                  "var42NN15", "var42NN30", "var42NN38",
                                                                  "var43NN15", "var43NN30", "var43NN38",
                                                                  "var45NN15", "var45NN30", "var45NN38"))]

train_temp <- full_data_temp[1:76020, ]
test_temp <- full_data_temp[76021:151838, ]
train_temp <- cbind(train_temp, train_prob)
test_temp <- cbind(test_temp, test_prob)

write.csv(train_temp, file = "train_set4_Enhanced.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set4_Enhanced.csv", row.names = FALSE)
