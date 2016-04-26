library(xgboost)
library(Matrix)
library(data.table)
#library(bit64, pos = .Machine$integer.max) #junk package that won't let use commercially and also won't let unmask functions even when you specify predescence
library(vtreat)
library(outliers)

remove(list = ls())
setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
train <- read.csv("train.csv")
test  <- read.csv("test.csv")
#train <- as.data.frame(fread("train.csv", header = TRUE, sep = ","))
#test <- as.data.frame(fread("test.csv", header = TRUE, sep = ","))

#unloadNamespace("bit64")
#detach("package:bit64", unload=TRUE, force=TRUE)

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
    if (all(train_temp[[f1]] == train_temp[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train_temp), toRemove)

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]


# ~~~~ GRADIENT DESCENT ASSOCIATION RULE TESTING
optimized_func <- function(target, scores, min_score, min_node, false_neg, cutoff) {
  # cutoff has two values: the minimum and maximum. Everything between is dished out.
  if (cutoff[2] < cutoff[1]) {
    known <- integer(0)
  } else {
    known <- target[which(((scores >= cutoff[2]) == TRUE) | ((scores <= cutoff[1]) == TRUE))] #takes values in exterior to [cutoff1, cutoff2], else dishes out empty numeric
  }
  if (length(known) >= min_node) { #if cutoff not leading to empty variable
    best <- ifelse(sum(known == 1) == 0, 0, ifelse(((sum(known == 0) / sum(known == 1)) >= min_score) & (sum(known == 1) <= false_neg), sum(known == 1) / sum(known == 0), 999)) #return 0 if pure rule, else return the probability if ratio over 25 (better than random), else return 999 (worse than random)
  } else {
    best <- 999 # node empty
  }
  return(best) # objective: return the purest node
}



# THIS IS FOR UNIVARIATE

prog_bar <- txtProgressBar(style = 3)
data_scores <- rbind(train_temp, test_temp)
for (i in colnames(train_temp)) {
  data_scores[[i]] <- scores(data_scores[[i]])
  #cat("Computed ", i, "'s scores. (", which(i == colnames(train_temp)), "/", length(colnames(train_temp)), ")\n", sep = "")
  setTxtProgressBar(prog_bar, which(i == colnames(train_temp))/length(colnames(train_temp)))
}
close(prog_bar)
data_scores_parsed <- data.frame(matrix(ncol = ncol(data_scores)+1, nrow = nrow(data_scores)))
colnames(data_scores_parsed) <- c(colnames(data_scores), "Final")
data_scores_parsed$Final <- rep(1, nrow(data_scores))
minimal_score <- 25 #don't accept any node under the allowed score
minimal_node <- 5 #don't accept any node containing under that specific amount of samples
false_negatives <- 2 #allow at most 1 false negative | higher allows a more permissive algorithm, lower makes it very difficult to converge

for (i in colnames(train_temp)) {
  data_scores_parsed[[i]] <- rep(1, nrow(data_scores))
  scoring_input <- data_scores[[i]][1:nrow(train)] #get scores from train set
  min_allowance <- min(scoring_input) #get the maximum allowed score
  max_allowance <- max(scoring_input) #get the maximum allowed score
  optimized_output <- optim(par = c(min_allowance, max_allowance), optimized_func, method = "L-BFGS-B", target = train_target, scores = scoring_input, min_score = minimal_score, min_node = minimal_node, false_neg = false_negatives, lower = min_allowance, upper = max_allowance, control = list(maxit = 1000, trace = 0))
  cat("[", which(i == colnames(train_temp)), ": ", i, "] ", ifelse(optimized_output$value >= 999, "Failed to optimize with gradient descent (you should loose conditions!).", paste("Best node: ", ifelse(optimized_output$value == 0, "Inf", 1/optimized_output$value), ":1 (", round(optimized_output$value*100, digits = 3) , "%) [ ", sum((scoring_input >= optimized_output$par[2]) | (scoring_input <= optimized_output$par[1])), "(train) | ", sum((data_scores[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] >= optimized_output$par[2]) | (data_scores[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] <= optimized_output$par[1])), "(test) ] for params (", optimized_output$par[1], ", ", optimized_output$par[2], ").", sep = "")), sep = "")
  if ((optimized_output$value >= 999) | (sum((data_scores[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] >= optimized_output$par[2]) | (data_scores[[i]][(nrow(train)+1):NROW(data_scores_parsed[[i]])] <= optimized_output$par[1]))) == 0) {
    #do nothing
    cat(" | was useless!\n", sep = "")
  } else {
    data_output <- ifelse(optimized_output$value == 0, 0, optimized_output$value)
    data_scores_parsed[[i]][(scoring_input >= optimized_output$par[2]) | (scoring_input <= optimized_output$par[1])] <- data_output
    data_scores_parsed$Final <- data_scores_parsed$Final * data_scores_parsed[[i]]
    cat(" | was stored!\n", sep = "")
  }
}
cat("\n-----\nSummary:\nTrain rows soft-ruled: ", nrow(train) - sum(data_scores_parsed$Final[1:nrow(train)] == 1), " (pure: ", sum(data_scores_parsed$Final[1:nrow(train)] == 0), ")\nTest rows soft-ruled: ", nrow(test) - sum(data_scores_parsed$Final[(nrow(train)+1):nrow(data_scores_parsed)] == 1), " (pure: ", sum(data_scores_parsed$Final[(nrow(train)+1):nrow(data_scores_parsed)] == 0), ")", sep = "")





# THIS IS FOR BIVARIATE

scored_rows <- rep(1, nrow(train_temp)+nrow(test_temp))
minimal_score <- 25 #don't accept any node under the allowed score
minimal_node <- 25 #don't accept any node containing under that specific amount of samples
false_negatives <- 0 #allow at most 1 false negative | higher allows a more permissive algorithm, lower makes it very difficult to converge

#for bivariate outliers = use Mahalonobis distance
Counter <- 0
MaxCounter <- ncol(train_temp)*(ncol(train_temp) - 1)
for (i in colnames(train_temp)) {
  
  for (j in colnames(train_temp)[-which(i == colnames(train_temp))]) {
    
    #print text for default checking
    Counter <- Counter + 1
    #tempText <- paste("\r[", ( (which(i == colnames(train_temp)) - 1) * ncol(train_temp) ) + ( which(j == colnames(train_temp))) , "/", ncol(train_temp)*ncol(train_temp), "]: ", i, ":", j, " parsed!", sep = "")
    tempText <- paste("\r[", Counter , "/", MaxCounter, "]: ", i, ":", j, " parsed!", sep = "")
    cat(tempText, sep = "")
    
    #merge columns
    #tempCol <- as.data.frame(cbind(c(train_temp[[i]], test_temp[[i]]), c(train_temp[[j]], test_temp[[j]])))
    tempCol <- data.frame(v1 = c(train_temp[[i]], test_temp[[i]]), v2 = c(train_temp[[j]], test_temp[[j]]), check.names = FALSE, stringsAsFactors = FALSE)
    
    #compute Mahalonobis distance (df, m, sx)
    tempCol <- mahalanobis(tempCol, colMeans(tempCol), cov(tempCol))
    
    #score the data against outliers locally
    data_scores <- scores(tempCol)
    scoring_input <- data_scores[1:nrow(train)] #get scores from train set
    min_allowance <- min(scoring_input) #get the maximum allowed score
    max_allowance <- max(scoring_input) #get the maximum allowed score
    
    #gradient descent the outliers to find local isolated nodes
    optimized_output <- optim(par = c(min_allowance, max_allowance), optimized_func, method = "L-BFGS-B", target = train_target, scores = scoring_input, min_score = minimal_score, min_node = minimal_node, false_neg = false_negatives, lower = min_allowance, upper = max_allowance, control = list(maxit = 1000, trace = 0))
    
    if (!(optimized_output$value == 0)) {
      
      #not pure node?
      #has no value for us
      #overwrite print
      
      cat("\r", rep(" ", nchar(tempText)), sep = "")
      
    } else {
      
      #pure node?
      #has value for us
      
      #compute rows found
      tempRows <- (data_scores >= optimized_output$par[2]) | (data_scores <= optimized_output$par[1])
      tempRows_train <- which(tempRows[1:nrow(train_temp)] == TRUE)
      tempRows_test <- which(tempRows[(nrow(train_temp)+1):(nrow(train_temp)+nrow(test_temp))] == TRUE)
      
      #update target rows
      tempInt <- scored_rows[scored_rows[(nrow(train_temp)+1):(nrow(train_temp)+nrow(test_temp))] == 0]
      scored_rows[tempRows] <- 0
      tempInt <- length(scored_rows[scored_rows[(nrow(train_temp)+1):(nrow(train_temp)+nrow(test_temp))] == 0]) - length(tempInt)
      
      #rewrite the current line
      cat("\r", rep(" ", nchar(tempText)), sep = "")
      #cat("\r[", ( (which(i == colnames(train_temp)) - 1) * ncol(train_temp) ) + ( which(j == colnames(train_temp))) , "/", ncol(train_temp)*ncol(train_temp), "]: ", i, ":", j, " analysis led to: ", length(tempRows_train), "|", length(tempRows_test), " (", length(scored_rows[scored_rows[1:nrow(train_temp)] == 0]), "|", length(scored_rows[scored_rows[(nrow(train_temp)+1):(nrow(train_temp)+nrow(test_temp))] == 0]), ")", sep = "")
      cat("\r[", Count , "/", MaxCounter, "]: ", i, ":", j, " analysis led to: ", length(tempRows_train), "|", length(tempRows_test), " (", length(scored_rows[scored_rows[1:nrow(train_temp)] == 0]), "|", length(scored_rows[scored_rows[(nrow(train_temp)+1):(nrow(train_temp)+nrow(test_temp))] == 0]), ")", sep = "")
      
      if (tempInt == 0) {
        
        #if it added nothing to our test set
        cat(" - No effect on test set.\n", sep = "")
        
      } else {
        
        #if it added something to our test set
        cat(" | improved slightly our score! (+", tempInt, ")\n", sep = "")
        
      }
      
    }
    
  }
  
}





# CHOOSE YOUR SUBMISSION!!!

submission  <- read.csv("submission_rules.csv") #take the best script?
tempInt <- which(data_scores_parsed$Final[76021:151838] == 0)
submission$TARGET[tempInt]
#submission$TARGET <- submission$TARGET * data_scores_parsed$Final[76021:151838]
submission$TARGET[submission$TARGET == 0] <- 0.00000001 #because we know some are wrong
submission$TARGET[tempInt] <- 0 #because we know we are right there
write.csv(submission, file = "submission_rules_out.csv", row.names = FALSE)
