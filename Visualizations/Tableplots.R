# run the following to create .jpg tableplots.
# WARNING: 370 output files

remove(list = ls())
setwd("C:/Users/Laurae/Documents/Data Science/Santander/")
train <- read.csv("train.csv")
test  <- read.csv("test.csv")

library(tableplot)
for (i in 2:371) {
  jpeg(filename = paste("./autoplots/plot_", i, ".jpg", sep = ""), width = 960, height = 960, units = "px", pointsize = 12)
  tableplot(dat = a, select = c(1,i), sortCol = target, sample = FALSE, nBins = 101)
  dev.off()
}
