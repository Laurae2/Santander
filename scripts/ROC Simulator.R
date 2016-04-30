RuleMaker <- function(data, rule1, rule2 = NA) {
  
  # data = the data set
  # rule1 = the first rule in form c("varName", lower, upper)
  # rule2 = the second rule in form c("varName", lower, upper)
  
  if (is.na(rule2[1]) == TRUE) {
    
    rule1_data <- data[[rule1[1]]]
    output <- rep(1, nrow(data))
    output[(rule1_data <= as.numeric(rule1[3])) & (rule1_data >= as.numeric(rule1[2]))] <- 0
    
  } else {
    
    rule1_data <- data[[rule1[1]]]
    rule2_data <- data[[rule2[1]]]
    output <- rep(1, nrow(data))
    output[(rule1_data <= as.numeric(rule1[3])) & (rule1_data >= as.numeric(rule1[2])) & (rule2_data <= as.numeric(rule2[3])) & (rule2_data >= as.numeric(rule2[2]))] <- 0
    
  }
  
  return(output)
  
}

FastROC <- function(y, x) {
  
  # y = actual
  # x = predicted
  x1 = x[y==1]
  n1 = length(x1)
  x2 = x[y==0]
  n2 = length(x2)
  r = rank(c(x1,x2))
  return((sum(r[1:n1]) - n1*(n1+1)/2) / (n1*n2))
  
}

SimulatorROC <- function(actual, predicted, ruled, prob = 0.00, kick = 0.0000001, target = 1, from = 0, to = floor(sum(ruled == 0)/(sum(actual == 0) / sum(actual == 1)) + 1), verbose = TRUE) {
  
  # actual = actual predictions
  # predicted = predicted predictions
  # ruled = ruled rows (TRUE/FALSE per row)
  # prob = what probability to settle for? (default = 0)
  # kick = any probabilities below that value is kicked to "kick" probability value (default = 0.. with many 0 and 1 to end)
  # target = what are we ruling out from? (what we don't want in our rules, i.e wrong values in rules)
  # from / to = how many positives to simulate? (worst + best + mean case are simulated) - 0 is always taken no matter what you want to do, ignored value
  # verbose = print output?
  
  # output: data frame with amount of false posiives, worst ROC, best ROC, mean ROC
  
  if (to < 3) {cat("You play in fire, don't try! (requires higher support rule, i.e ", floor(to * (sum(actual == 0) / sum(actual == 1)))); return()}
  
  Paster <- paste("%0", nchar(to, "width"), "d", sep = "")
  which_rules <- which(ruled == 0)
  ruled.min <- min(which_rules)
  ruled.max <- max(which_rules)
  ruled.count <- NROW(which_rules)
  probabilities <- predicted
  probabilities[probabilities < kick] <- kick
  
  temp_df <- data.frame(FalseAbs = rep(0, (to - from + 1)),
                        FalseRel = rep(0, (to - from + 1)),
                        FalseRelAdj = rep(0, (to - from + 1)),
                        Prob = rep(0, (to - from + 1)),
                        ROC = rep(0, (to - from + 1)))
                        #MeanAUC = rep(0, (to - from + 1)),
                        #MinAUC = rep(0, (to - from + 1)),
                        #MaxAUC = rep(0, (to - from + 1))
  
  baseROC <- auc(actual, predicted)
  
  # do for from = 0
  temp_probs <- probabilities
  temp_probs[which_rules] <- prob
  temp_df[1, 1] <- 0
  temp_df[2, 1] <- 0
  temp_df[3, 1] <- 0
  temp_df[4, 1] <- prob
  temp_df[5, 1] <- auc(actual, temp_probs)
  #temp_df[6, 1] <- auc(actual, temp_probs)
  #temp_df[7, 1] <- auc(actual, temp_probs)
  
  if (verbose == TRUE) {
    cat("Base ROC to beat: ", sprintf("%.06f", round(baseROC, digits = 6)), "\n")
    #cat("Falses in rule: ", temp_df[1, 1], "(", sprintf("%.03f", 100*temp_df[2, 1]), "% | Adj: ", sprintf("%.03f", 100*temp_df[3, 1]), "%) with prob = ", temp_df[4, 1], " ===> ROC = ", sprintf("%.06f", round(temp_df[5, 1], digits = 6)), "[", sprintf("%.06f", round(temp_df[6, 1], digits = 6)), ", ", sprintf("%.06f", round(temp_df[7, 1], digits = 6)), "]\n", sep = "")
    cat("Falses in rule: ", sprintf(Paster, temp_df[1, 1]), "(", ifelse(temp_df[5, 1] > baseROC, "+", "-"), ") => ROC = ", sprintf("%.06f", round(temp_df[5, 1], digits = 6)), "  (", sprintf("%.03f", 100*temp_df[2, 1]), "% | Adj: ", sprintf("%.03f", 100*temp_df[3, 1]), "%) with prob = ", temp_df[4, 1], "\n", sep = "")
  }
  
  
  for (i in (from+1):to) {
    
    temp_probs <- probabilities
    temp_probs[which_rules] <- prob
    #fake_actual1 <- actual
    #fake_actual1[which_rules[seq(1, i)]] <- target
    #fake_actual2 <- actual
    #fake_actual2[which_rules[seq(ruled.count, i)[1:i]]] <- target
    fake_actual <- actual
    fake_actual[which_rules[seq(1, i)]] <- target
    
    temp_df[1, i] <- i
    temp_df[2, i] <- i/ruled.count
    temp_df[3, i] <- (i/ruled.count) * (sum(actual == 0) / sum(actual == 1))
    temp_df[4, i] <- prob
    #temp_df[6, i] <- auc(fake_actual1, temp_probs)
    #temp_df[7, i] <- auc(fake_actual2, temp_probs)
    #temp_df[5, i] <- (temp_df[6, i] + temp_df[7, i]) / 2
    temp_df[5, i] <- auc(fake_actual, temp_probs)
    
    if (verbose == TRUE) {
      #cat("Falses in rule: ", temp_df[1, i], "(", sprintf("%.03f", 100*temp_df[2, i]), "% | Adj: ", sprintf("%.03f", 100*temp_df[3, i]), "%) with prob = ", temp_df[4, i], " ===> ROC = ", sprintf("%.06f", round(temp_df[5, i], digits = 6)), "[", sprintf("%.06f", round(temp_df[6, i], digits = 6)), ", ", sprintf("%.06f", round(temp_df[7, i], digits = 6)), "]\n", sep = "")
      cat("Falses in rule: ", sprintf(Paster, temp_df[1, i]), "(", ifelse(temp_df[5, i] > baseROC, "+", "-"), ") => ROC = ", sprintf("%.06f", round(temp_df[5, i], digits = 6)), "  (", sprintf("%.03f", 100*temp_df[2, i]), "% | Adj: ", sprintf("%.03f", 100*temp_df[3, i]), "%) with prob = ", temp_df[4, i], "\n", sep = "")
    }
    
    
  }
  
}

# SimulatorROCBoot <- function(actual, predicted, ruled, prob = 0.00, kick = 0.0000001, target = 1, from = 0, to = floor(sum(ruled == 0)/(sum(actual == 0) / sum(actual == 1)) + 1), verbose = TRUE, size = 0.50, batch = 10, boot = 100, seed = 0) {
#   
#   # actual = actual predictions
#   # predicted = predicted predictions
#   # ruled = ruled rows (TRUE/FALSE per row)
#   # prob = what probability to settle for? (default = 0)
#   # kick = any probabilities below that value is kicked to "kick" probability value (default = 0.. with many 0 and 1 to end)
#   # target = what are we ruling out from? (what we don't want in our rules, i.e wrong values in rules)
#   # from / to = how many positives to simulate? (worst + best + mean case are simulated) - 0 is always taken no matter what you want to do, ignored value
#   # verbose = print output?
#   # size = sample size per iteration?
#   # batch = how many batches of iterations?
#   # boot = how many iterations?
#   # seed = seed to use?
#   
#   # output: data frame with amount of false posiives, worst ROC, best ROC, mean ROC
#   
#   if (to < 3) {cat("You play in fire, don't try! (requires higher support rule, i.e ", floor(to * (sum(actual == 0) / sum(actual == 1)))); return()}
#   
#   Paster <- paste("%0", nchar(to, "width"), "d", sep = "")
#   which_rules <- which(ruled == 0)
#   ruled.min <- min(which_rules)
#   ruled.max <- max(which_rules)
#   ruled.count <- NROW(which_rules)
#   probabilities <- predicted
#   probabilities[probabilities < kick] <- kick
#   actual0 <- which(actual == 0)
#   actual0.count <- length(actual0)
#   FastSeq0 <- ceiling(actual0.count*size)
#   actual1 <- which(actual == 1)
#   actual1.count <- length(actual1)
#   FastSeq1 <- ceiling(actual1.count*size)
#   ratio <- actual0.count/actual1.count
#   FastSeq <- seq(1, ruled.count)
#   
#   baseROC <- auc(actual, predicted)
#   
#   temp_probs <- probabilities
#   temp_ROC <- rep(0, boot*batch)
#   prog_bar <- txtProgressBar(style = 3)
#   
#   for (j in 1:batch) {
#     
#     set.seed(j+seed)
#     #part <- createDataPartition(actual, times = boot, p = size, list = FALSE)
#     for (k in 1:boot) {
#       #temp_ROC[(j-1)*batch+boot] <- FastROC(actual[part[, k]], predicted[part[, k]])
#       set.seed(k+seed)
#       # part1 <- rnorm(actual0.count, 0, 1)
#       # part1 <- actual0[which(part1 < sort(part1, method = "quick")[FastSeq0])]
#       # part2 <- rnorm(actual1.count, 0, 1)
#       # part2 <- actual1[which(part2 < sort(part2, method = "quick")[FastSeq1])]
#       part1 <- actual0[sample(actual0.count, FastSeq0)]
#       part2 <- actual1[sample(actual1.count, FastSeq1)]
#       parts <- c(part1, part2)
#       temp_ROC[(j-1)*boot+k] <- FastROC(actual[parts], predicted[parts])
#       # temp_ROC[(j-1)*boot+k] <- FastROC(c(actual[part1], actual[part2]), c(predicted[part1], predicted[part2]))
#     }
#     setTxtProgressBar(prog_bar, j/batch)
#     
#   }
#   
#   close(prog_bar)
#   
#   meanROC <- mean(temp_ROC)
#   sdROC <- sd(temp_ROC)
#   minROC <- min(temp_ROC)
#   maxROC <- max(temp_ROC)
#   
#   if (verbose == TRUE) {
#     cat("\nCurrent ROC: ", baseROC, " | from Simulation: ", sprintf("%.06f", round(meanROC, digits = 6)), "+", sprintf("%.06f", round(sdROC, digits = 6)), "[", sprintf("%.06f", round(minROC, digits = 6)), ", ", sprintf("%.06f", round(maxROC, digits = 6)), "]\n\n", sep = "")
#   }
#   
#   bootROC <- meanROC
#   
#   for (i in from:to) {
# 
#     temp_probs <- probabilities
#     temp_probs[which_rules] <- prob
#     temp_ROC <- rep(0, boot*batch)
#     prog_bar <- txtProgressBar(style = 3)
# 
#     for (j in 1:batch) {
# 
#       set.seed(j+seed)
#       #part <- createDataPartition(actual, times = boot, p = size, list = FALSE)
#       for (k in 1:boot) {
#         fake_actual <- actual
#         set.seed(k+seed)
#         fake_actual[sample(which_rules, i)] <- target
#         # temp_ROC[(j-1)*boot+k] <- FastROC(fake_actual[part[, k]], temp_probs[part[, k]])
#         set.seed(k+seed)
#         part1 <- actual0[sample(actual0.count, FastSeq0)]
#         part2 <- actual1[sample(actual1.count, FastSeq1)]
#         parts <- c(part1, part2)
#         temp_ROC[(j-1)*boot+k] <- FastROC(fake_actual[parts], predicted[parts])
# 
#       }
#       setTxtProgressBar(prog_bar, j/batch)
# 
#     }
# 
#     close(prog_bar)
# 
#     meanROC <- mean(temp_ROC)
#     sdROC <- sd(temp_ROC)
#     minROC <- min(temp_ROC)
#     maxROC <- max(temp_ROC)
# 
#     if (verbose == TRUE) {
#       cat("Falses in rule: ", sprintf(Paster, i), "(", ifelse(meanROC > baseROC, "+", "-"), ifelse(bootROC > baseROC, "+", "-"), ") => ROC = ", sprintf("%.06f", round(meanROC, digits = 6)), "+", sprintf("%.06f", round(sdROC, digits = 6)), "[", sprintf("%.06f", round(minROC, digits = 6)), ", ", sprintf("%.06f", round(maxROC, digits = 6)), "]  (", sprintf("%.03f", 100*i/ruled.count), "% | Adj: ", sprintf("%.03f", 100*((i/ruled.count) * (sum(actual == 0) / sum(actual == 1)))), "%) with prob = ", prob, "\n\n", sep = "")
#     }
# 
#   }
#   
# }




SimulatorROCBoot <- function(actual, predicted, ruled, prob = 0.00, kick = 0.0000001, target = 1, from = 0, to = floor(sum(ruled == 0)/(sum(actual == 0) / sum(actual == 1)) + 1), verbose = TRUE, size = 0.50, batch = 10, boot = 100, seed = 0) {
  
  # actual = actual predictions
  # predicted = predicted predictions
  # ruled = ruled rows (TRUE/FALSE per row)
  # prob = what probability to settle for? (default = 0)
  # kick = any probabilities below that value is kicked to "kick" probability value (default = 0.. with many 0 and 1 to end)
  # target = what are we ruling out from? (what we don't want in our rules, i.e wrong values in rules)
  # from / to = how many positives to simulate? (worst + best + mean case are simulated) - 0 is always taken no matter what you want to do, ignored value
  # verbose = print output?
  # size = sample size per iteration?
  # batch = how many batches of iterations?
  # boot = how many iterations?
  # seed = seed to use?
  
  # output: data frame with amount of false posiives, worst ROC, best ROC, mean ROC
  
  if (to < 3) {cat("You play in fire, don't try! (requires higher support rule, i.e ", floor(to * (sum(actual == 0) / sum(actual == 1)))); return()}
  
  Paster <- paste("%0", nchar(to, "width"), "d", sep = "")
  which_rules <- which(ruled == 0)
  ruled.min <- min(which_rules)
  ruled.max <- max(which_rules)
  ruled.count <- NROW(which_rules)
  probabilities <- predicted
  probabilities[probabilities < kick] <- kick
  actual0 <- which(actual == 0)
  actual0.count <- length(actual0)
  FastSeq0 <- ceiling(actual0.count*size)
  actual1 <- which(actual == 1)
  actual1.count <- length(actual1)
  FastSeq1 <- ceiling(actual1.count*size)
  ratio <- actual0.count/actual1.count
  FastSeq <- seq(1, ruled.count)
  
  baseROC <- auc(actual, predicted)
  
  temp_probs <- probabilities
  temp_ROC <- rep(0, boot*batch)
  
  for (j in 1:batch) {
    
    set.seed(j+seed)
    for (k in 1:boot) {
      set.seed(j*boot+k+seed)
      part1 <- actual0[sample(actual0.count, FastSeq0)]
      part2 <- actual1[sample(actual1.count, FastSeq1)]
      parts <- c(part1, part2)
      temp_ROC[(j-1)*boot+k] <- FastROC(actual[parts], predicted[parts])
    }
    if (verbose == TRUE) {
      meanROC <- mean(temp_ROC[1:(j*boot)])
      sdROC <- sd(temp_ROC[1:(j*boot)])
      minROC <- min(temp_ROC[1:(j*boot)])
      maxROC <- max(temp_ROC[1:(j*boot)])
      cat("\rCurrent ROC:", sprintf("%.07f", round(baseROC, digits = 7)), "(", ifelse(meanROC > baseROC, "+", "-"), ") | from Simulation: ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "] (", round(j/batch*100, digits = 0), "%)", sep = "")
    }
    
  }
  
  meanROC <- mean(temp_ROC)
  sdROC <- sd(temp_ROC)
  minROC <- min(temp_ROC)
  maxROC <- max(temp_ROC)
  
  if (verbose == TRUE) {
    cat("\rCurrent ROC:", sprintf("%.07f", round(baseROC, digits = 7)), "(", ifelse(meanROC > baseROC, "+", "-"), ") | from Simulation: ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "]       \n\n", sep = "")
  }
  
  bootROC <- meanROC
  
  for (i in from:to) {
    
    temp_probs <- probabilities
    temp_probs[which_rules] <- prob
    temp_ROC <- rep(0, boot*batch)
    
    for (j in 1:batch) {
      
      set.seed(j+seed)
      for (k in 1:boot) {
        fake_actual <- actual
        set.seed(j*boot+k+seed)
        fake_actual[sample(which_rules, i)] <- target
        set.seed(j*boot+k+seed)
        part1 <- actual0[sample(actual0.count, FastSeq0)]
        part2 <- actual1[sample(actual1.count, FastSeq1)]
        parts <- c(part1, part2)
        temp_ROC[(j-1)*boot+k] <- FastROC(fake_actual[parts], temp_probs[parts])
        
      }
      if (verbose == TRUE) {
        meanROC <- mean(temp_ROC[1:(j*boot)])
        sdROC <- sd(temp_ROC[1:(j*boot)])
        minROC <- min(temp_ROC[1:(j*boot)])
        maxROC <- max(temp_ROC[1:(j*boot)])
        cat("\rFalses in rule: ", sprintf(Paster, i), "(", ifelse(meanROC > baseROC, "+", "-"), ifelse(bootROC > baseROC, "+", "-"), ") => ROC = ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "]  (", sprintf("%.03f", 100*i/ruled.count), "% | Adj: ", sprintf("%.03f", 100*((i/ruled.count) * (sum(actual == 0) / sum(actual == 1)))), "%) with prob = ", prob, " (", round(j/batch*100, digits = 0), "%)", sep = "")
      }
      
    }
    
    meanROC <- mean(temp_ROC)
    sdROC <- sd(temp_ROC)
    minROC <- min(temp_ROC)
    maxROC <- max(temp_ROC)
    
    if (verbose == TRUE) {
      cat("\rFalses in rule: ", sprintf(Paster, i), "(", ifelse(meanROC > baseROC, "+", "-"), ifelse(bootROC > baseROC, "+", "-"), ") => ROC = ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "]  (", sprintf("%.03f", 100*i/ruled.count), "% | Adj: ", sprintf("%.03f", 100*((i/ruled.count) * (sum(actual == 0) / sum(actual == 1)))), "%) with prob = ", prob, "       \n", sep = "")
    }
    
  }
  
}


SimulatorROCProbBoot <- function(actual, predicted, ruled, falseAbs = 1, prob = c(0.00, 0.01, 0.02, 0.03), kick = 0.0000001, target = 1, verbose = TRUE, size = 0.50, batch = 10, boot = 100, seed = 0, ROCBase = NA) {
  
  # actual = actual predictions
  # predicted = predicted predictions
  # ruled = ruled rows (TRUE/FALSE per row)
  # prob = what probability to test for? (default = c(0.00, 0.01, 0.02, 0.03))
  # kick = any probabilities below that value is kicked to "kick" probability value (default = 0.. with many 0 and 1 to end)
  # target = what are we ruling out from? (what we don't want in our rules, i.e wrong values in rules)
  # falseAbs = what is the false absolute rate? (for what amount are you optimizing?)
  # verbose = print output?
  # size = sample size per iteration?
  # batch = how many batches of iterations?
  # boot = how many iterations?
  # seed = seed to use?
  
  # output: data frame with amount of false posiives, worst ROC, best ROC, mean ROC
  
  which_rules <- which(ruled == 0)
  ruled.min <- min(which_rules)
  ruled.max <- max(which_rules)
  ruled.count <- NROW(which_rules)
  probabilities <- predicted
  probabilities[probabilities < kick] <- kick
  actual0 <- which(actual == 0)
  actual0.count <- length(actual0)
  FastSeq0 <- ceiling(actual0.count*size)
  actual1 <- which(actual == 1)
  actual1.count <- length(actual1)
  FastSeq1 <- ceiling(actual1.count*size)
  ratio <- actual0.count/actual1.count
  FastSeq <- seq(1, ruled.count)
  
  baseROC <- auc(actual, predicted)
  
  temp_probs <- probabilities
  temp_ROC <- rep(0, boot*batch)
  
  if (is.na(ROCBase[1])) {
    
    for (j in 1:batch) {
      
      set.seed(j+seed)
      for (k in 1:boot) {
        set.seed(j*boot+k+seed)
        part1 <- actual0[sample(actual0.count, FastSeq0)]
        part2 <- actual1[sample(actual1.count, FastSeq1)]
        parts <- c(part1, part2)
        temp_ROC[(j-1)*boot+k] <- FastROC(actual[parts], predicted[parts])
      }
      if (verbose == TRUE) {
        meanROC <- mean(temp_ROC[1:(j*boot)])
        sdROC <- sd(temp_ROC[1:(j*boot)])
        minROC <- min(temp_ROC[1:(j*boot)])
        maxROC <- max(temp_ROC[1:(j*boot)])
        cat("\rCurrent ROC:", sprintf("%.07f", round(baseROC, digits = 7)), "(", ifelse(meanROC > baseROC, "+", "-"), ") | from Simulation: ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "] (", round(j/batch*100, digits = 0), "%)", sep = "")
      }
      
    }
    
    meanROC <- mean(temp_ROC)
    sdROC <- sd(temp_ROC)
    minROC <- min(temp_ROC)
    maxROC <- max(temp_ROC)
    
    if (verbose == TRUE) {
      cat("\rCurrent ROC:", sprintf("%.07f", round(baseROC, digits = 7)), "(", ifelse(meanROC > baseROC, "+", "-"), ") | from Simulation: ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "]       \n", sep = "")
    }
    
    bootROC <- meanROC
    
  } else {
    
    meanROC <- ROCBase[1]
    bootROC <- ROCBase[2]
    
  }
  
  if (!(is.na(prob))) {
    
    for (i in 1:NROW(prob)) {
      
      temp_probs <- probabilities
      temp_probs[which_rules] <- prob[i]
      temp_ROC <- rep(0, boot*batch)
      
      for (j in 1:batch) {
        
        set.seed(j+seed)
        for (k in 1:boot) {
          fake_actual <- actual
          set.seed(j*boot+k+seed)
          fake_actual[sample(which_rules, i)] <- target
          set.seed(j*boot+k+seed)
          part1 <- actual0[sample(actual0.count, FastSeq0)]
          part2 <- actual1[sample(actual1.count, FastSeq1)]
          parts <- c(part1, part2)
          temp_ROC[(j-1)*boot+k] <- FastROC(fake_actual[parts], temp_probs[parts])
          
        }
        if (verbose == TRUE) {
          meanROC <- mean(temp_ROC[1:(j*boot)])
          sdROC <- sd(temp_ROC[1:(j*boot)])
          minROC <- min(temp_ROC[1:(j*boot)])
          maxROC <- max(temp_ROC[1:(j*boot)])
          cat("\rFalses in rule: ", falseAbs, "(", ifelse(meanROC > baseROC, "+", "-"), ifelse(bootROC > baseROC, "+", "-"), ") => ROC = ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "]  (", sprintf("%.03f", 100*i/ruled.count), "% | Adj: ", sprintf("%.03f", 100*((i/ruled.count) * (sum(actual == 0) / sum(actual == 1)))), "%) with prob = ", sprintf("%.08f", prob[i]), " (", round(j/batch*100, digits = 0), "%)", sep = "")
        }
        
      }
      
      meanROC <- mean(temp_ROC)
      sdROC <- sd(temp_ROC)
      minROC <- min(temp_ROC)
      maxROC <- max(temp_ROC)
      
      if (verbose == TRUE) {
        cat("\rFalses in rule: ", falseAbs, "(", ifelse(meanROC > baseROC, "+", "-"), ifelse(bootROC > baseROC, "+", "-"), ") => ROC = ", sprintf("%.07f", round(meanROC, digits = 7)), "+", sprintf("%.07f", round(sdROC, digits = 7)), "[", sprintf("%.07f", round(minROC, digits = 7)), ", ", sprintf("%.07f", round(maxROC, digits = 7)), "]  (", sprintf("%.03f", 100*i/ruled.count), "% | Adj: ", sprintf("%.03f", 100*((i/ruled.count) * (sum(actual == 0) / sum(actual == 1)))), "%) with prob = ", sprintf("%.08f", prob[i]), "       \n", sep = "")
      }
      
    }
    
    return(1-meanROC)
    
  } else {
    
    return(c(meanROC, bootROC))
    
  }
  
}

optimize_ROC <- function(actual, predicted, ruled, falseAbs, lower = 0, upper = 0.5, iter = 15, ...) {
  
  ROCBase <- SimulatorROCProbBoot(actual, predicted, ruled, prob = NA, ...)
  optimized_output <- optim(par = 0, SimulatorROCProbBoot, method = "Brent", actual = actual, predicted = predicted, ruled = ruled, falseAbs = falseAbs, ..., ROCBase = ROCBase, lower = lower, upper = upper, control = list(maxit = iter, trace = 0))
  return(optimized_output)
  
}
