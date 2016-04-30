library(xgboost)
library(caret)

setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
source("Santander Feature Creator.R") #change to appropriate local file name

train <- read.csv("train.csv")
test  <- read.csv("test.csv")
full_data <- rbind(train[, -371], test)
full_data[full_data == 9999999999] <- 0
full_data[full_data[, "var3"] == -999999, "var3"] <- -1

full_data <- cbind(full_data, CreateFeatures(full_data, "var1"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var2"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var5"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var7"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var8"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var9"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var10"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var12"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var13"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var14"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var16"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var17"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var18"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var20"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var22"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var24"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var25"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var26"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var27"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var28"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var29"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var30"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var31"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var32"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var33"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var34"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var37"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var39"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var40"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var41"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var43"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var44"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var45"))
full_data <- cbind(full_data, CreateFeatures(full_data, "var46"))
full_data <- cbind(full_data, CreateFeatureSets(full_data))

train_temp <- full_data[1:76020, ]
test_temp <- full_data[76021:151838, ]


# removing ID
train_temp$ID <- NULL
test_temp$ID <- NULL

# extracting label
train_target <- train$TARGET

# Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train_temp)) {
  if (length(unique(train_temp[[f]])) == 1) {
    cat(f, "is constant.\n")
    train_temp[[f]] <- NULL
    test_temp[[f]] <- NULL
  }
}

# Removing identical features
features_pair <- combn(names(train_temp), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train_temp[[f1]] == train_temp[[f2]])) {
      cat(f1, "and", f2, "are equal.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train_temp), toRemove)

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]


set.seed(11111) #set.seed(2016)
folds <- createMultiFolds(train_target, k = 4, times = 1)

input_folds_xgb <- list()
for (i in names(folds)) {
  input_folds_xgb[[i]] <- seq(1, 76020)[-folds[[i]]]
}

dtrain <- xgb.DMatrix(data = data.matrix(train_temp), label = train_target)

gc()
set.seed(11111)
clf.cv <- xgb.cv(eta     = 0.05,
                 max_depth  = 5,
                 subsample  = 0.70,
                 #colsample  = 0.10,
                 colsample_bytree= 0.70,
                 data       = dtrain, 
                 nrounds    = 50000, 
                 verbose    = 1,
                 #gamma      = 15,
                 maximize   = TRUE,
                 nthread    = 4,
                 early.stop.round = 50,
                 #nfold      = 4,
                 folds      = input_folds_xgb,
                 objective  = "binary:logistic",
                 booster    = "gbtree",
                 eval_metric= "auc"
)
