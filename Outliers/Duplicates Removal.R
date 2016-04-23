# the following should print all positives which have duplicates either positive and negative, and put them into temp_mutuals
# WARNING: takes a while to run, because I didn't both vectorizing it

remove(list = ls())
setwd("C:/Users/Laurae/Documents/Data Science/Santander/")
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

train_temp <- train[, -1]

temp_i <- which(train_temp$TARGET == 1)
excluded_index <- seq(1, nrow(train_temp))
excluded_index[temp_i] <- -1
temp_mutuals <- data.frame(Row = seq(1, nrow(train_temp)), Target = train_temp$TARGET, Duplicate = rep(FALSE, nrow(train_temp)), Differing = rep(FALSE, nrow(train_temp)))
sink("debloat.R, append = FALSE, split = FALSE)
for (i in temp_i) {
  tempStr <- "\n"
  temp_j <- which(!(excluded_index == -1))
  #prog_bar <- txtProgressBar(style = 3)
  # for (j in temp_j) {
  #   if (identical(train_temp[j, -370], train_temp[i, -370])) {
  #     tempStr <- paste(tempStr, "Duplicate: ", i, "(", train_temp[i, 370], ") equal to ", j, "(", train_temp[j, 370], ")\n", sep = "")
  #     excluded_index[j] <- -1
  #   }
  #   setTxtProgressBar(prog_bar, j/NROW(temp_j))
  # }
  tempInt <- rep(0, NROW(temp_j))
  for (j in 1:369) {
    tempInt <- tempInt + (train_temp[temp_j, j] == train_temp[i, j])
    #cat(sum(tempInt), "\n")
    #setTxtProgressBar(prog_bar, j/369)
  }
  #close(prog_bar)
  if (sum(tempInt == 369) == 0) {} else {
    #cat("\nInspecting ", i, "\n", sep = "")
    temp_j <- which(tempInt == 369)
    excluded_index[temp_j] <- -1
    temp_mutuals[temp_j, "Duplicate"] <- TRUE
    #prog_bar <- txtProgressBar(style = 3)
    temp_positives <- 0
    temp_negatives <- 0
    tempScript <- "train <- train[-c("
    tempList1 <- ""
    tempList2 <- ""
    for (j in temp_j) {
      if (train_temp[i, 370] == train_temp[j, 370]) {
        temp_positives <- temp_positives + 1
        tempList1 <- paste(tempList1, ifelse(tempList1 == "", j, paste(", ", j, sep = "")), sep = "")
        tempStr <- paste(tempStr, "Duplicate: ", i, "(", train_temp[i, 370], ") equal to ", j, "(", train_temp[j, 370], ")\n", sep = "")
      } else {
        temp_negatives <- temp_negatives + 1
        tempList2 <- paste(tempList2, ifelse(tempList2 == "", j, paste(", ", j, sep = "")), sep = "")
        tempStr <- paste(tempStr, "Duplicate (***Differing***): ", i, "(", train_temp[i, 370], ") equal to ", j, "(", train_temp[j, 370], ")\n", sep = "")
        temp_mutuals[j, "Differing"] <- TRUE
      }
      #setTxtProgressBar(prog_bar, j/NROW(temp_j))
    }
    tempScript <- paste(tempScript, ifelse(temp_negatives/24 < (temp_positives+1), paste(tempList2, ", ", tempList1, sep = ""), tempList1), "), ]\n", sep = "")
    cat(tempScript, sep = "")
    #close(prog_bar)
  }
}
sink()
