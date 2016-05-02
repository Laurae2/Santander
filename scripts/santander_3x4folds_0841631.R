library(caret)
library(R.utils)

library(pROC)

#setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
#train_temp <- train ##set train_temp to the appropriate training set
#train_target <- target ##set train_target to the label

train <- read.csv("train.csv")
test  <- read.csv("test.csv")

train_temp <- train
test_temp <- test
train_target <- train$TARGET

##### Removing IDs
train_temp$ID <- NULL
test.id <- test_temp$ID
test_temp$ID <- NULL

##### Extracting TARGET
train.y <- train_temp$TARGET
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
    if (all(train_temp[[f1]] == train_temp[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train_temp), toRemove)

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]


set.seed(11111)
folds <- createMultiFolds(train_target, k = 4, times = 3)

score_list <- data.frame(folds = seq(1, NROW(names(folds))), scores = rep(0, NROW(names(folds))), rounds = rep(0, NROW(names(folds))))
StartTime <- System$currentTimeMillis()
for (i in names(folds)) {
  gc(verbose = FALSE)
  training_xgb <- xgb.DMatrix(data = data.matrix(train_temp[folds[[i]], ]), label = train_target[folds[[i]]])
  gc(verbose = FALSE)
  testing_xgb <- xgb.DMatrix(data = data.matrix(train_temp[-folds[[i]], ]), label = train_target[-folds[[i]]])
  gc(verbose = FALSE)
  set.seed(11111)
  sink(file = "junktext.txt", append = TRUE, split = FALSE) #BECAUSE xgboost 0.47 doesn't let verbose = 0, HOW BAD!
  best_out <- xgb.train(eta     = 0.02,
                        max_depth  = 5,
                        subsample  = 0.70,
                        colsample_bytree= 0.70,
                        data       = training_xgb, 
                        nrounds    = 50000, 
                        verbose    = 0,
                        maximize   = TRUE,
                        nthread    = 31,
                        early.stop.round = 100,
                        print.every.n = 50,
                        objective  = "binary:logistic",
                        booster    = "gbtree",
                        eval_metric= "auc",
                        watchlist = list(test = testing_xgb, train = training_xgb))
  sink()
  gc(verbose = FALSE)
  score_list[which(i == names(folds)), "scores"] <- best_out$bestScore
  score_list[which(i == names(folds)), "rounds"] <- best_out$bestInd
  CurrentTime <- System$currentTimeMillis()
  SpentTime <- (CurrentTime - StartTime) / 1000
  cat("[Fold ", sprintf("%02d", which(i == names(folds))), "/", sprintf("%02d", NROW(names(folds))), " | CPU: ", sprintf("%.02f", SpentTime), "s | ETA: ", sprintf("%.02f", (NROW(names(folds)) - which(i == names(folds))) * SpentTime / which(i == names(folds))), "s]: test-AUC=", sprintf("%.06f", best_out$bestScore), " (", sprintf("%04d", best_out$bestInd), " rounds). Currently M/SD: ", sprintf("%.03f", mean(score_list[1:(which(i == names(folds))), 2])), "+", sprintf("%.03f", sd(score_list[1:(which(i == names(folds))), 2])), "\n", sep = "")
}
cat("-----\nFinal results (M/SD): ", sprintf("%.06f", mean(score_list[, 2])), "+", sprintf("%.06f", sd(score_list[, 2])), " with ", sprintf("04d", mean(score_list[, 3])), "+", sprintf("04d", sd(score_list[, 3])), " rounds", sep = "")

# [Fold 01/12 | CPU: 23.99s | ETA: 263.90s]: test-AUC=0.838210 (0419 rounds). Currently M/SD: 0.838+NA
# [Fold 02/12 | CPU: 45.10s | ETA: 225.52s]: test-AUC=0.836749 (0366 rounds). Currently M/SD: 0.837+0.001
# [Fold 03/12 | CPU: 74.87s | ETA: 224.61s]: test-AUC=0.850082 (0563 rounds). Currently M/SD: 0.842+0.007
# [Fold 04/12 | CPU: 96.29s | ETA: 192.59s]: test-AUC=0.843005 (0372 rounds). Currently M/SD: 0.842+0.006
# [Fold 05/12 | CPU: 113.18s | ETA: 158.45s]: test-AUC=0.839025 (0282 rounds). Currently M/SD: 0.841+0.005
# [Fold 06/12 | CPU: 136.19s | ETA: 136.19s]: test-AUC=0.846880 (0410 rounds). Currently M/SD: 0.842+0.005
# [Fold 07/12 | CPU: 157.28s | ETA: 112.34s]: test-AUC=0.843955 (0362 rounds). Currently M/SD: 0.843+0.005
# [Fold 08/12 | CPU: 180.34s | ETA: 90.17s]: test-AUC=0.835301 (0411 rounds). Currently M/SD: 0.842+0.005
# [Fold 09/12 | CPU: 194.17s | ETA: 64.72s]: test-AUC=0.839458 (0209 rounds). Currently M/SD: 0.841+0.005
# [Fold 10/12 | CPU: 219.89s | ETA: 43.98s]: test-AUC=0.845575 (0478 rounds). Currently M/SD: 0.842+0.005
# [Fold 11/12 | CPU: 243.77s | ETA: 22.16s]: test-AUC=0.849263 (0430 rounds). Currently M/SD: 0.843+0.005
# [Fold 12/12 | CPU: 267.27s | ETA: 0.00s]: test-AUC=0.832066 (0446 rounds). Currently M/SD: 0.842+0.006
# -----
# Final results (M/SD): 0.841631+0.005710 with 04d+04d rounds
