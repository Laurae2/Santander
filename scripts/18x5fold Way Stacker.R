library(xgboost)
library(Matrix)
library(caret)

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

# For (Fold01, Fold1): best score was 0.841329 at 384 rounds. Out of 602 positives, 397 (65.95%) were categorized correctly.
# For (Fold01, Fold2): best score was 0.829726 at 628 rounds. Out of 601 positives, 389 (64.73%) were categorized correctly.
# For (Fold01, Fold3): best score was 0.845194 at 352 rounds. Out of 602 positives, 401 (66.61%) were categorized correctly.
# For (Fold01, Fold4): best score was 0.843381 at 224 rounds. Out of 602 positives, 393 (65.28%) were categorized correctly.
# For (Fold01, Fold5): best score was 0.843374 at 475 rounds. Out of 601 positives, 411 (68.39%) were categorized correctly.
# For (Fold02, Fold1): best score was 0.841447 at 407 rounds. Out of 602 positives, 400 (66.45%) were categorized correctly.
# For (Fold02, Fold2): best score was 0.832694 at 616 rounds. Out of 601 positives, 389 (64.73%) were categorized correctly.
# For (Fold02, Fold3): best score was 0.843618 at 503 rounds. Out of 602 positives, 401 (66.61%) were categorized correctly.
# For (Fold02, Fold4): best score was 0.843099 at 330 rounds. Out of 602 positives, 398 (66.11%) were categorized correctly.
# For (Fold02, Fold5): best score was 0.843334 at 523 rounds. Out of 601 positives, 409 (68.05%) were categorized correctly.
# For (Fold03, Fold1): best score was 0.839897 at 398 rounds. Out of 602 positives, 401 (66.61%) were categorized correctly.
# For (Fold03, Fold2): best score was 0.83065 at 449 rounds. Out of 601 positives, 387 (64.39%) were categorized correctly.
# For (Fold03, Fold3): best score was 0.844015 at 348 rounds. Out of 602 positives, 399 (66.28%) were categorized correctly.
# For (Fold03, Fold4): best score was 0.845221 at 268 rounds. Out of 602 positives, 395 (65.61%) were categorized correctly.
# For (Fold03, Fold5): best score was 0.842396 at 368 rounds. Out of 601 positives, 410 (68.22%) were categorized correctly.
# For (Fold04, Fold1): best score was 0.840735 at 429 rounds. Out of 602 positives, 409 (67.94%) were categorized correctly.
# For (Fold04, Fold2): best score was 0.831397 at 658 rounds. Out of 601 positives, 395 (65.72%) were categorized correctly.
# For (Fold04, Fold3): best score was 0.84421 at 466 rounds. Out of 602 positives, 402 (66.78%) were categorized correctly.
# For (Fold04, Fold4): best score was 0.843981 at 517 rounds. Out of 602 positives, 409 (67.94%) were categorized correctly.
# For (Fold04, Fold5): best score was 0.842438 at 344 rounds. Out of 601 positives, 411 (68.39%) were categorized correctly.
# For (Fold05, Fold1): best score was 0.840765 at 437 rounds. Out of 602 positives, 400 (66.45%) were categorized correctly.
# For (Fold05, Fold2): best score was 0.827749 at 551 rounds. Out of 601 positives, 391 (65.06%) were categorized correctly.
# For (Fold05, Fold3): best score was 0.843167 at 314 rounds. Out of 602 positives, 397 (65.95%) were categorized correctly.
# For (Fold05, Fold4): best score was 0.843782 at 271 rounds. Out of 602 positives, 399 (66.28%) were categorized correctly.
# For (Fold05, Fold5): best score was 0.843853 at 498 rounds. Out of 601 positives, 416 (69.22%) were categorized correctly.
# For (Fold06, Fold1): best score was 0.842043 at 340 rounds. Out of 602 positives, 393 (65.28%) were categorized correctly.
# For (Fold06, Fold2): best score was 0.830036 at 525 rounds. Out of 601 positives, 385 (64.06%) were categorized correctly.
# For (Fold06, Fold3): best score was 0.845704 at 469 rounds. Out of 602 positives, 390 (64.78%) were categorized correctly.
# For (Fold06, Fold4): best score was 0.844646 at 225 rounds. Out of 602 positives, 386 (64.12%) were categorized correctly.
# For (Fold06, Fold5): best score was 0.841729 at 437 rounds. Out of 601 positives, 419 (69.72%) were categorized correctly.
# For (Fold07, Fold1): best score was 0.841537 at 422 rounds. Out of 602 positives, 396 (65.78%) were categorized correctly.
# For (Fold07, Fold2): best score was 0.831176 at 546 rounds. Out of 601 positives, 377 (62.73%) were categorized correctly.
# For (Fold07, Fold3): best score was 0.845195 at 393 rounds. Out of 602 positives, 392 (65.12%) were categorized correctly.
# For (Fold07, Fold4): best score was 0.846816 at 793 rounds. Out of 602 positives, 402 (66.78%) were categorized correctly.
# For (Fold07, Fold5): best score was 0.843275 at 386 rounds. Out of 601 positives, 403 (67.05%) were categorized correctly.
# For (Fold08, Fold1): best score was 0.840298 at 360 rounds. Out of 602 positives, 396 (65.78%) were categorized correctly.
# For (Fold08, Fold2): best score was 0.834412 at 616 rounds. Out of 601 positives, 404 (67.22%) were categorized correctly.
# For (Fold08, Fold3): best score was 0.843872 at 485 rounds. Out of 602 positives, 400 (66.45%) were categorized correctly.
# For (Fold08, Fold4): best score was 0.842378 at 310 rounds. Out of 602 positives, 392 (65.12%) were categorized correctly.
# For (Fold08, Fold5): best score was 0.839068 at 73 rounds. Out of 601 positives, 406 (67.55%) were categorized correctly.
# For (Fold09, Fold1): best score was 0.841713 at 443 rounds. Out of 602 positives, 402 (66.78%) were categorized correctly.
# For (Fold09, Fold2): best score was 0.828866 at 536 rounds. Out of 601 positives, 383 (63.73%) were categorized correctly.
# For (Fold09, Fold3): best score was 0.846384 at 459 rounds. Out of 602 positives, 399 (66.28%) were categorized correctly.
# For (Fold09, Fold4): best score was 0.844785 at 264 rounds. Out of 602 positives, 394 (65.45%) were categorized correctly.
# For (Fold09, Fold5): best score was 0.841026 at 440 rounds. Out of 601 positives, 407 (67.72%) were categorized correctly.
# For (Fold10, Fold1): best score was 0.840736 at 371 rounds. Out of 602 positives, 404 (67.11%) were categorized correctly.
# For (Fold10, Fold2): best score was 0.829815 at 569 rounds. Out of 601 positives, 390 (64.89%) were categorized correctly.
# For (Fold10, Fold3): best score was 0.844406 at 322 rounds. Out of 602 positives, 396 (65.78%) were categorized correctly.
# For (Fold10, Fold4): best score was 0.84486 at 538 rounds. Out of 602 positives, 406 (67.44%) were categorized correctly.
# For (Fold10, Fold5): best score was 0.842815 at 438 rounds. Out of 601 positives, 410 (68.22%) were categorized correctly.
# For (Fold11, Fold1): best score was 0.84195 at 399 rounds. Out of 602 positives, 405 (67.28%) were categorized correctly.
# For (Fold11, Fold2): best score was 0.830535 at 567 rounds. Out of 601 positives, 387 (64.39%) were categorized correctly.
# For (Fold11, Fold3): best score was 0.844236 at 460 rounds. Out of 602 positives, 398 (66.11%) were categorized correctly.
# For (Fold11, Fold4): best score was 0.842557 at 374 rounds. Out of 602 positives, 396 (65.78%) were categorized correctly.
# For (Fold11, Fold5): best score was 0.841326 at 215 rounds. Out of 601 positives, 406 (67.55%) were categorized correctly.
# For (Fold12, Fold1): best score was 0.840404 at 460 rounds. Out of 602 positives, 402 (66.78%) were categorized correctly.
# For (Fold12, Fold2): best score was 0.831111 at 765 rounds. Out of 601 positives, 391 (65.06%) were categorized correctly.
# For (Fold12, Fold3): best score was 0.843545 at 514 rounds. Out of 602 positives, 390 (64.78%) were categorized correctly.
# For (Fold12, Fold4): best score was 0.84163 at 443 rounds. Out of 602 positives, 406 (67.44%) were categorized correctly.
# For (Fold12, Fold5): best score was 0.841807 at 464 rounds. Out of 601 positives, 407 (67.72%) were categorized correctly.
# For (Fold13, Fold1): best score was 0.84073 at 340 rounds. Out of 602 positives, 395 (65.61%) were categorized correctly.
# For (Fold13, Fold2): best score was 0.83013 at 620 rounds. Out of 601 positives, 391 (65.06%) were categorized correctly.
# For (Fold13, Fold3): best score was 0.847397 at 474 rounds. Out of 602 positives, 400 (66.45%) were categorized correctly.
# For (Fold13, Fold4): best score was 0.84349 at 593 rounds. Out of 602 positives, 407 (67.61%) were categorized correctly.
# For (Fold13, Fold5): best score was 0.842971 at 331 rounds. Out of 601 positives, 412 (68.55%) were categorized correctly.
# For (Fold14, Fold1): best score was 0.837905 at 419 rounds. Out of 602 positives, 402 (66.78%) were categorized correctly.
# For (Fold14, Fold2): best score was 0.832704 at 584 rounds. Out of 601 positives, 389 (64.73%) were categorized correctly.
# For (Fold14, Fold3): best score was 0.84554 at 374 rounds. Out of 602 positives, 399 (66.28%) were categorized correctly.
# For (Fold14, Fold4): best score was 0.844832 at 544 rounds. Out of 602 positives, 410 (68.11%) were categorized correctly.
# For (Fold14, Fold5): best score was 0.843676 at 552 rounds. Out of 601 positives, 419 (69.72%) were categorized correctly.
# For (Fold15, Fold1): best score was 0.84011 at 371 rounds. Out of 602 positives, 405 (67.28%) were categorized correctly.
# For (Fold15, Fold2): best score was 0.831059 at 467 rounds. Out of 601 positives, 388 (64.56%) were categorized correctly.
# For (Fold15, Fold3): best score was 0.845271 at 401 rounds. Out of 602 positives, 392 (65.12%) were categorized correctly.
# For (Fold15, Fold4): best score was 0.841359 at 234 rounds. Out of 602 positives, 399 (66.28%) were categorized correctly.
# For (Fold15, Fold5): best score was 0.841933 at 432 rounds. Out of 601 positives, 414 (68.89%) were categorized correctly.
# For (Fold16, Fold1): best score was 0.842742 at 385 rounds. Out of 602 positives, 402 (66.78%) were categorized correctly.
# For (Fold16, Fold2): best score was 0.830422 at 376 rounds. Out of 601 positives, 385 (64.06%) were categorized correctly.
# For (Fold16, Fold3): best score was 0.844676 at 308 rounds. Out of 602 positives, 394 (65.45%) were categorized correctly.
# For (Fold16, Fold4): best score was 0.844212 at 329 rounds. Out of 602 positives, 405 (67.28%) were categorized correctly.
# For (Fold16, Fold5): best score was 0.841867 at 484 rounds. Out of 601 positives, 421 (70.05%) were categorized correctly.
# For (Fold17, Fold1): best score was 0.84075 at 378 rounds. Out of 602 positives, 399 (66.28%) were categorized correctly.
# For (Fold17, Fold2): best score was 0.830962 at 533 rounds. Out of 601 positives, 383 (63.73%) were categorized correctly.
# For (Fold17, Fold3): best score was 0.846488 at 485 rounds. Out of 602 positives, 396 (65.78%) were categorized correctly.
# For (Fold17, Fold4): best score was 0.8447 at 450 rounds. Out of 602 positives, 397 (65.95%) were categorized correctly.
# For (Fold17, Fold5): best score was 0.843336 at 352 rounds. Out of 601 positives, 406 (67.55%) were categorized correctly.
# For (Fold18, Fold1): best score was 0.841079 at 361 rounds. Out of 602 positives, 406 (67.44%) were categorized correctly.
# For (Fold18, Fold2): best score was 0.828531 at 462 rounds. Out of 601 positives, 382 (63.56%) were categorized correctly.
# For (Fold18, Fold3): best score was 0.844831 at 475 rounds. Out of 602 positives, 397 (65.95%) were categorized correctly.
# For (Fold18, Fold4): best score was 0.842765 at 474 rounds. Out of 602 positives, 403 (66.94%) were categorized correctly.
# For (Fold18, Fold5): best score was 0.842009 at 334 rounds. Out of 601 positives, 416 (69.22%) were categorized correctly.
