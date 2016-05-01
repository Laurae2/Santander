remove(list = ls())
setwd("C:/Users/Laurae/Documents/Data Science/Santander/") ##set your own working directory
train <- read.csv("train.csv")
test  <- read.csv("test.csv")




# Generate Set 1
# 307 features, 0.842 Public LB

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
    if ((all(train_temp[[f1]] == train_temp[[f2]]))) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train_temp), toRemove)

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]

train_temp$TARGET <- train_target

write.csv(train_temp, file = "train_set1.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set1.csv", row.names = FALSE)




# Generate Set 2
# 10 features, 0.837 LB
# Added n0, removed std

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

feature.names <- c("var15", "saldo_var30", "n0", "num_var22_ult3", "imp_op_var39_ult1", "num_var45_hace3", "saldo_medio_var5_hace2", "var3", "saldo_medio_var8_ult3", "ind_var41_0")

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]

train_temp$TARGET <- train_target

write.csv(train_temp, file = "train_set2.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set2.csv", row.names = FALSE)



# Generate Set 3
# 62 features, 0.841 LB
# Removed PCA

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

feature.names <- c('num_var39_0',  # 0.00031104199066874026
                   'ind_var13',  # 0.00031104199066874026
                   'num_op_var41_comer_ult3',  # 0.00031104199066874026
                   'num_var43_recib_ult1',  # 0.00031104199066874026
                   'imp_op_var41_comer_ult3',  # 0.00031104199066874026
                   'num_var8',  # 0.00031104199066874026
                   'num_var42',  # 0.00031104199066874026
                   'num_var30',  # 0.00031104199066874026
                   'saldo_var8',  # 0.00031104199066874026
                   'num_op_var39_efect_ult3',  # 0.00031104199066874026
                   'num_op_var39_comer_ult3',  # 0.00031104199066874026
                   'num_var41_0',  # 0.0006220839813374805
                   'num_op_var39_ult3',  # 0.0006220839813374805
                   'saldo_var13',  # 0.0009331259720062209
                   'num_var30_0',  # 0.0009331259720062209
                   'ind_var37_cte',  # 0.0009331259720062209
                   'ind_var39_0',  # 0.001244167962674961
                   'num_var5',  # 0.0015552099533437014
                   'ind_var10_ult1',  # 0.0015552099533437014
                   'num_op_var39_hace2',  # 0.0018662519440124418
                   'num_var22_hace2',  # 0.0018662519440124418
                   'num_var35',  # 0.0018662519440124418
                   'ind_var30',  # 0.0018662519440124418
                   'num_med_var22_ult3',  # 0.002177293934681182
                   'imp_op_var41_efect_ult1',  # 0.002488335925349922
                   'var36',  # 0.0027993779160186624
                   'num_med_var45_ult3',  # 0.003110419906687403
                   'imp_op_var39_ult1',  # 0.0037325038880248835
                   'imp_op_var39_comer_ult3',  # 0.0037325038880248835
                   'imp_trans_var37_ult1',  # 0.004043545878693624
                   'num_var5_0',  # 0.004043545878693624
                   'num_var45_ult1',  # 0.004665629860031105
                   'ind_var41_0',  # 0.0052877138413685845
                   'imp_op_var41_ult1',  # 0.0052877138413685845
                   'num_var8_0',  # 0.005598755832037325
                   'imp_op_var41_efect_ult3',  # 0.007153965785381027
                   'num_op_var41_ult3',  # 0.007153965785381027
                   'num_var22_hace3',  # 0.008087091757387248
                   'num_var4',  # 0.008087091757387248
                   'imp_op_var39_comer_ult1',  # 0.008398133748055987
                   'num_var45_ult3',  # 0.008709175738724729
                   'ind_var5',  # 0.009953343701399688
                   'imp_op_var39_efect_ult3',  # 0.009953343701399688
                   'num_meses_var5_ult3',  # 0.009953343701399688
                   'saldo_var42',  # 0.01181959564541213
                   'imp_op_var39_efect_ult1',  # 0.013374805598755831
                   'num_var45_hace2',  # 0.014618973561430793
                   'num_var22_ult1',  # 0.017107309486780714
                   'saldo_medio_var5_ult1',  # 0.017418351477449457
                   'saldo_var5',  # 0.0208398133748056
                   'ind_var8_0',  # 0.021150855365474338
                   'ind_var5_0',  # 0.02177293934681182
                   'num_meses_var39_vig_ult3',  # 0.024572317262830483
                   'saldo_medio_var5_ult3',  # 0.024883359253499222
                   'num_var45_hace3',  # 0.026749611197511663
                   'num_var22_ult3',  # 0.03452566096423017
                   'saldo_medio_var5_hace3',  # 0.04074650077760498
                   'saldo_medio_var5_hace2',  # 0.04292379471228616
                   'n0',  # 0.04696734059097978
                   'saldo_var30',  # 0.09611197511664074
                   'var38',  # 0.1390357698289269
                   'var15')  # 0.20964230171073095

train_temp <- train_temp[, feature.names]
test_temp <- test_temp[, feature.names]

train_temp$TARGET <- train_target

write.csv(train_temp, file = "train_set3.csv", row.names = FALSE)
write.csv(test_temp, file = "test_set3.csv", row.names = FALSE)
