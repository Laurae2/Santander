# factor finder
train_temp <- train[, -371]
test_temp <- test
full_data <- rbind(train_temp, test_temp)
df_temp <- data.frame(VarName = rep(0, 370), Factors = rep(0, 370))
class(df_temp$VarName) <- "character"
for (i in colnames(full_data)) {
  j <- which(colnames(full_data) == i)
  df_temp[j, 1] <- i
  df_temp[j, 2] <- NROW(unique(full_data[, i]))
  cat(i, ": ", df_temp[j ,2], " unique values.\n", sep = "")
}
df_temp <- df_temp[order(-df_temp[, 2]), ]
