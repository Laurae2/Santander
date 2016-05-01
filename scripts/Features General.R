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
    if ((all(train_temp[[f1]] == train_temp[[f2]])) | (sum(cor(train_temp[, c(f1, f2)])) > 3.999)) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}
