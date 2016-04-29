library(xgboost)
library(Matrix)
library(data.table)
library(R.utils)
library(caret)

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


#findCorrelation(cor(full_data_temp), cutoff = .999, verbose = TRUE, names = TRUE)

train <- read.csv("train.csv")
test  <- read.csv("test.csv")


train_temp <- train
test_temp <- test

# removing ID
train_temp$ID <- NULL
test_temp$ID <- NULL

# extracting label
train_target <- train$TARGET
train_temp$TARGET <- NULL

# 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train_temp$n0 <- apply(train_temp, 1, FUN=count0)
test_temp$n0 <- apply(test_temp, 1, FUN=count0)

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

full_data_temp <- cbind(full_data_temp, rbind(train_temp, test_temp))

train_temp <- full_data_temp[1:76020, ]
test_temp <- full_data_temp[76021:151838, ]
