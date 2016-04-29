transformProbabilitiesToRanking <- function(...) {
  dots <- list(...)
  ndots <- length(dots)
  ranks <- as.data.table(test$ID)
  for(i in 1:ndots){
    dots[[i]] <- as.data.table(dots[[i]])
    dots[[i]][,rank:=rank(-TARGET,ties.method="average")]
    ranks[,paste0(i)] <- dots[[i]]$rank
  }
  ranks = as.data.frame(ranks)
  ranks[,"sum"] <- rowSums(ranks[, -1], na.rm = FALSE, dims = 1)
  sum_min <- min(ranks$sum)
  sum_max <- max(ranks$sum)
  ranks[,"norm"] <- (ranks$sum - sum_min)/(sum_max - sum_min)
  return(data.frame(ID=ranks$V1,TARGET=ranks$norm))
}

# Our top submission with rules
sub1 <- read.csv("./submissions/submission_rules_out.csv")
sub2 <- read.csv("./submissions/submission_rules_out1.csv")
sub3 <- read.csv("submission_ensemble4.csv")
sub4 <- read.csv("submission_ensemble2.csv")
sub5 <- read.csv("submissions_ens_4_pow_4.csv")
sub6 <- read.csv("./submissions/submission.csv")
#https://www.kaggle.com/yuansun/santander-customer-satisfaction/lb-0-84-for-starters
sub7 <- read.csv("./submissions/submission (1).csv")
#https://www.kaggle.com/dmi3kno/santander-customer-satisfaction/people-with-loans-never-complain/code

submission_rank <- transformProbabilitiesToRanking(sub1, sub6, sub7)