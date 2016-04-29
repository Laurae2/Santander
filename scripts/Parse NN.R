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
