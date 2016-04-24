library(xgboost)
library(Matrix)

remove(list = ls())
setwd("C:/Users/Laurae/Documents/Data Science/Santander/")
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

train_temp <- train
test_temp <- test

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

train_temp$TARGET <- train.y

library(caret)
train_positives <- train_temp[train_temp$TARGET == 1, ]
train_negatives <- train_temp[train_temp$TARGET == 0, ]

j <- 1
k <- 1
#training_list <- list()
training_folds <- list()
input_folds_xgb <- list()
for (i in 1:18) {
  if (i %% 4 == 0) {
    k <- k + 4056
  } else {
    k <- k + 4055
  }
  #training_list[[paste("Fold", sprintf("%02d", i), sep = "")]] <- train_negatives[j:k, ]
  training_folds[[paste("Fold", sprintf("%02d", i), sep = "")]] <- seq(j, k, 1)
  input_folds_xgb[[i]] <- seq(1, 73012)[-training_folds[[paste("Fold", sprintf("%02d", i), sep = "")]]]
  k <- k + 1
  j <- k
  cat("j=", j, " | k=", k, " ", sep = "")
}

tested <- xgb.DMatrix(data = data.matrix(test_temp))
gc(verbose=FALSE)
preds_out <- rep(0, nrow(test_temp))
for (i in names(training_folds)) {
  set.seed(11111)
  folds <- createFolds(seq(1, 3008), k = 5, list = TRUE)
  for (j in names(folds)) {
    #dtrain <- xgb.DMatrix(data = data.matrix(rbind(training_list[[i]][, -308], train_positives[, -308])), label = c(training_list[[i]]$TARGET, train_positives$TARGET))
    dtrain <- xgb.DMatrix(data = data.matrix(rbind(train_positives[-folds[[j]], -308], train_negatives[training_folds[[i]], -308])), label = c(train_positives$TARGET[-folds[[j]]], train_negatives$TARGET[training_folds[[i]]]))
    gc(verbose=FALSE)
    dtest <- xgb.DMatrix(data = data.matrix(rbind(train_positives[folds[[j]], -308], train_negatives[-training_folds[[i]], -308])), label = c(train_positives$TARGET[folds[[j]]], train_negatives$TARGET[-training_folds[[i]]]))
    gc(verbose=FALSE)
    watchlist <- list(test = dtest, train = dtrain)
    # dcv <- xgb.DMatrix(data = data.matrix(train_temp[, -308]), label = c(train_temp$TARGET))
    # gc(verbose=FALSE)
    # set.seed(11111)
    # clf.cv <- xgb.cv(eta     = 0.05,
    #                  max_depth  = 5,
    #                  subsample  = 0.65,
    #                  colsample_bytree= 0.7,
    #                  data       = dtrain, 
    #                  nrounds    = 50000, 
    #                  verbose    = 2,
    #                  maximize   = TRUE,
    #                  nthread    = 4,
    #                  early.stop.round = 50,
    #                  #nfold      = 4,
    #                  folds      = input_folds_xgb[[i]],
    #                  objective  = "binary:logistic",
    #                  booster    = "gbtree",
    #                  eval_metric= "auc"
    #                  )
    # cat("Best performance on ", i, ": ", max(clf.cv$test.auc.mean)[1], " (round ", max(clf.cv$test.auc.mean)[1], ")", sep = "")
    gc(verbose=FALSE)
    sink(file = "junktext.txt", append = FALSE, split = FALSE) #BECAUSE xgboost 0.47 doesn't let verbose = 0, HOW BAD!
    set.seed(11111)
    clf <- xgb.train(   eta        = 0.01,
                        max_depth  = 5,
                        subsample  = 0.7,
                        colsample_bytree= 0.7,
                        nthread    = 4,
                        data       = dtrain, 
                        nrounds    = 1000000, 
                        verbose    = 0,
                        watchlist  = watchlist,
                        maximize   = TRUE,
                        early.stop.round = 50,
                        objective  = "binary:logistic",
                        booster    = "gbtree",
                        eval_metric= "auc"
    )
    gc(verbose=FALSE)
    sink()
    cat("For (", i, ", ", j, "): best score was ", clf$bestScore, " at ", clf$bestInd, " rounds. ", sep = "")
    preds_in <- predict(clf, newdata = xgb.DMatrix(data.matrix(train_positives[folds[[j]], -308])), ntreelimit = clf$bestInd)
    cat("Out of ", NROW(preds_in), " positives, ", sum(preds_in >= 0.50), " (", round(100 * sum(preds_in >= 0.50) / NROW(preds_in), digits = 2), "%) were categorized correctly.\n", sep = "")
    preds_out <- preds_out + predict(clf, newdata = tested, ntreelimit = clf$bestInd)
  }
}
preds_out <- preds_out / (NROW(names(training_folds)) * NROW(names(folds)))

submission <- data.frame(ID=test.id, TARGET=preds_out)
cat("saving the submission file\n")
write.csv(submission, "submission.csv", row.names = F)
