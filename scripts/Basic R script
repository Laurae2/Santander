#from the published script

library(xgboost)
library(Matrix)

remove(list = ls())
setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
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


train_temp2 <- sparse.model.matrix(TARGET ~ ., data = train_temp)

dtrain <- xgb.DMatrix(data=train_temp2, label=train.y)
watchlist <- list(train=dtrain)
test_temp$TARGET <- -1
test_temp2 <- sparse.model.matrix(TARGET ~ ., data = test_temp)

set.seed(11111)

clf.cv <- xgb.cv(eta     = 0.001,
                 max_depth  = 5,
                 subsample  = 0.7,
                 colsample_bytree= 0.7,
                 data       = dtrain, 
                 nrounds    = 50000, 
                 verbose    = 2,
                 maximize   = TRUE,
                 nthread    = 4,
                 early.stop.round = 500,
                 nfold      = 8,
                 objective  = "binary:logistic",
                 booster    = "gbtree",
                 eval_metric= "auc"
)

set.seed(11111)

# 559 rounds 0.002 eta -> 0.840219
#1534 rounds 0.005 eta -> 0.840651
#7742 rounds 0.001 eta -> 0.840716 (~0.842 local)

clf <- xgb.train(   eta     = 0.02,
                    max_depth  = 5,
                    subsample  = 0.7,
                    colsample_bytree= 0.7,
                    nthread    = 2,
                    data       = dtrain, 
                    nrounds    = 559, 
                    verbose    = 2,
                    watchlist  = watchlist,
                    maximize   = TRUE,
                    objective  = "binary:logistic",
                    booster    = "gbtree",
                    eval_metric= "auc"
)


preds <- predict(clf, test_temp2)
submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submission, "submission.csv", row.names = F)
