# WARNING
# WARNING
# This creates 2 sets of data: fulldata.csv with all the data, and fulldata_parsed.csv with the engineered features and cleaned data
# Remember to remove the target variable afterwards


library(xgboost)
library(Matrix)
library(data.table)

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

full_data <- cbind(target = c(train_target, rep(-1, 75818)), rbind(train_temp, test_temp))

write.csv(full_data, file = "fulldata.csv", row.names = FALSE)

fixed_data <- data.frame(target = full_data[, "target"])

# Variable 1

fixed_data <- cbind(fixed_data,
                    full_data[, c("saldo_var1", "num_var1_0", "num_var1")],
                    saldo_var1ANDsaldo_var1 = (full_data[, "saldo_var1"] - 48.4491) * (full_data[, "saldo_var1"]), 
                    num_var1_0ANDnum_var1_0 = (full_data[, "num_var1_0"] - 0.03445) * (full_data[, "num_var1_0"] - 0.03445)
                    )


# Variable 2



# Variable 3

fixed_data <- cbind(fixed_data,
                    var3 = full_data[, "var3"],
                    var3ANDvar3 = (full_data[, "var3"] + 1523.2) * (full_data[, "var3"] + 1523.2)
                    )


# Variable 4

fixed_data <- cbind(fixed_data,
                    num_var4 = full_data[, "num_var4"],
                    num_var4ANDnum_var4 = (full_data[, "num_var4"] - 1.07944) * (full_data[, "num_var4"] - 1.07944),
                    num_var4ANDnum_var4ANDnum_var4 = (full_data[, "num_var4"] - 1.07944) * (full_data[, "num_var4"] - 1.07944) * (full_data[, "num_var4"] - 1.07944)
                    )


# Variable 5

fixed_data <- cbind(fixed_data,
                    full_data[, c("num_var5_0", "num_var5", "saldo_var5", "num_meses_var5_ult3", "saldo_medio_var5_hace2", "saldo_medio_var5_hace3", "saldo_medio_var5_ult1", "saldo_medio_var5_ult3")],
                    num_var5_0ANDnum_var5_0 = (full_data[, "num_var5_0"] - 2.89404) * (full_data[, "num_var5_0"] - 2.89404),
                    num_var5_0ANDnum_var5 = (full_data[, "num_var5_0"] - 2.89404) * (full_data[, "num_var5"] - 1.99917),
                    num_var5_0ANDsaldo_var5 = (full_data[, "num_var5_0"] - 2.89404) * (full_data[, "saldo_var5"] - 1028.47),
                    num_var5_0ANDnum_meses_var5_ult3 = (full_data[, "num_var5_0"] - 2.89404) * (full_data[, "num_meses_var5_ult3"] - 1.97998),
                    num_var5_0ANDsaldo_medio_var5_hace2 = (full_data[, "num_var5_0"] - 2.89404) * (full_data[, "saldo_medio_var5_hace2"] - 1579.14),
                    num_var5_0ANDsaldo_medio_var5_ult1 = (full_data[, "num_var5_0"] - 2.89404) * (full_data[, "saldo_medio_var5_ult1"] - 1077.26),
                    num_var5_0ANDsaldo_medio_var5_ult3 = (full_data[, "num_var5_0"] - 2.89404) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86),
                    num_var5ANDnum_var5 = (full_data[, "num_var5"] - 1.99917) * (full_data[, "num_var5"] - 1.99917),
                    num_var5ANDsaldo_var5 = (full_data[, "num_var5"] - 1.99917) * (full_data[, "saldo_var5"] - 1028.47),
                    num_var5ANDnum_meses_var5_ult3 = (full_data[, "num_var5"] - 1.99917) * (full_data[, "num_meses_var5_ult3"] - 1.97998),
                    num_var5ANDsaldo_medio_var5_hace2 = (full_data[, "num_var5"] - 1.99917) * (full_data[, "saldo_medio_var5_hace2"] - 1579.14),
                    num_var5ANDsaldo_medio_var5_hace3 = (full_data[, "num_var5"] - 1.99917) * (full_data[, "saldo_medio_var5_hace3"] - 891.366),
                    num_var5ANDsaldo_medio_var5_ult1 = (full_data[, "num_var5"] - 1.99917) * (full_data[, "saldo_medio_var5_ult1"] - 1077.26),
                    num_var5ANDsaldo_medio_var5_ult3 = (full_data[, "num_var5"] - 1.99917) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86),
                    saldo_var5ANDsaldo_var5 = (full_data[, "saldo_var5"] - 1028.47) * (full_data[, "saldo_var5"] - 1028.47),
                    saldo_var5ANDnum_meses_var5_ult3 = (full_data[, "saldo_var5"] - 1028.47) * (full_data[, "num_meses_var5_ult3"] - 1.97998),
                    saldo_var5ANDsaldo_medio_var5_hace2 = (full_data[, "saldo_var5"] - 1028.47) * (full_data[, "saldo_medio_var5_hace2"] - 1579.14),
                    saldo_var5ANDsaldo_medio_var5_hace3 = (full_data[, "saldo_var5"] - 1028.47) * (full_data[, "saldo_medio_var5_hace3"] - 891.366),
                    saldo_var5ANDsaldo_medio_var5_ult1 = (full_data[, "saldo_var5"] - 1028.47) * (full_data[, "saldo_medio_var5_ult1"] - 1077.26),
                    saldo_var5ANDsaldo_medio_var5_ult3 = (full_data[, "saldo_var5"] - 1028.47) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86),
                    num_meses_var5_ult3ANDnum_meses_var5_ult3 = (full_data[, "num_meses_var5_ult3"] - 1.97998) * (full_data[, "num_meses_var5_ult3"] - 1.97998),
                    num_meses_var5_ult3ANDsaldo_medio_var5_hace3 = (full_data[, "num_meses_var5_ult3"] - 1.97998) * (full_data[, "saldo_medio_var5_hace3"] - 891.366),
                    num_meses_var5_ult3ANDsaldo_medio_var5_ult1 = (full_data[, "num_meses_var5_ult3"] - 1.97998) * (full_data[, "saldo_medio_var5_ult1"] - 1077.26),
                    num_meses_var5_ult3ANDsaldo_medio_var5_ult3 = (full_data[, "num_meses_var5_ult3"] - 1.97998) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86),
                    saldo_medio_var5_hace2ANDsaldo_medio_var5_hace2 = (full_data[, "saldo_medio_var5_hace2"] - 1579.14) * (full_data[, "saldo_medio_var5_hace2"] - 1579.14),
                    saldo_medio_var5_hace2ANDsaldo_medio_var5_ult1 = (full_data[, "saldo_medio_var5_hace2"] - 1579.14) * (full_data[, "saldo_medio_var5_ult1"] - 1077.26),
                    saldo_medio_var5_hace2ANDsaldo_medio_var5_ult3 = (full_data[, "saldo_medio_var5_hace2"] - 1579.14) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86),
                    saldo_medio_var5_hace3ANDsaldo_medio_var5_hace3 = (full_data[, "saldo_medio_var5_hace3"] - 891.366) * (full_data[, "saldo_medio_var5_hace3"] - 891.366),
                    saldo_medio_var5_hace3ANDsaldo_medio_var5_ult1 = (full_data[, "saldo_medio_var5_hace3"] - 891.366) * (full_data[, "saldo_medio_var5_ult1"] - 1077.26),
                    saldo_medio_var5_hace3ANDsaldo_medio_var5_ult3 = (full_data[, "saldo_medio_var5_hace3"] - 891.366) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86),
                    saldo_medio_var5_ult1ANDsaldo_medio_var5_ult1 = (full_data[, "saldo_medio_var5_ult1"] - 1077.26) * (full_data[, "saldo_medio_var5_ult1"] - 1077.26),
                    saldo_medio_var5_ult1ANDsaldo_medio_var5_ult3 = (full_data[, "saldo_medio_var5_ult1"] - 1077.26) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86),
                    saldo_medio_var5_ult3ANDsaldo_medio_var5_ult3 = (full_data[, "saldo_medio_var5_ult3"] - 1048.86) * (full_data[, "saldo_medio_var5_ult3"] - 1048.86)
                    )


# Variable 6



# Variable 7



# Variable 8

fixed_data <- cbind(fixed_data,
                    full_data[, c("num_var8_0", "num_var8", "saldo_var8", "saldo_medio_var8_hace2")]
                    )


# Variable 9

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var9_cte_ult1", "ind_var9_ult1")]
                    )


# Variable 10

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var10_ult1", "ind_var10cte_ult1")]
                    )


# Variable 11

fixed_data <- cbind(fixed_data,
                    num_trasp_var11_ult1 = full_data[, c("num_trasp_var11_ult1")]
                    )


# Variable 12

fixed_data <- cbind(fixed_data,
                   full_data[, c("ind_var12_0", "num_var12", "saldo_var12", "num_meses_var12_ult3")]
                   )


# Variable 13

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var13_0", "num_aport_var13_hace3")]
                    )


# Variable 14

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var14_0", "ind_var14", "saldo_var14")]
                    )


# Variable 15

fixed_data <- cbind(fixed_data,
                    var15 = full_data[, c("var15")],
                    var15ANDvar15 = (full_data[, c("var15")] - 33.2129) * (full_data[, c("var15")] - 33.2129),
                    var15ANDvar15ANDvar15 = (full_data[, c("var15")] - 33.2129) * (full_data[, c("var15")] - 33.2129) * (full_data[, c("var15")] - 33.2129)
                    )


# Variable 16

fixed_data <- cbind(fixed_data,
                    full_data[, c("imp_sal_var16_ult1", "num_sal_var16_ult1", "imp_ent_var16_ult1", "num_ent_var16_ult1")],
                    imp_sal_var16_ult1ANDimp_sal_var16_ult1 = (full_data[, "imp_sal_var16_ult1"] - 5.47768) * (full_data[, "imp_sal_var16_ult1"] - 5.47768),
                    imp_sal_var16_ult1ANDnum_sal_var16_ult1 = (full_data[, "imp_sal_var16_ult1"] - 5.47768) * (full_data[, "num_sal_var16_ult1"] - 0.00493),
                    imp_sal_var16_ult1ANDimp_ent_var16_ult1 = (full_data[, "imp_sal_var16_ult1"] - 5.47768) * (full_data[, "imp_ent_var16_ult1"] - 86.2083),
                    imp_sal_var16_ult1ANDnum_ent_var16_ult1 = (full_data[, "imp_sal_var16_ult1"] - 5.47768) * (full_data[, "num_ent_var16_ult1"] - 0.18796),
                    num_sal_var16_ult1ANDnum_sal_var16_ult1 = (full_data[, "num_sal_var16_ult1"] - 0.00493) * (full_data[, "num_sal_var16_ult1"] - 0.00493),
                    num_sal_var16_ult1ANDimp_ent_var16_ult1 = (full_data[, "num_sal_var16_ult1"] - 0.00493) * (full_data[, "imp_ent_var16_ult1"] - 86.2083),
                    num_sal_var16_ult1ANDnum_ent_var16_ult1 = (full_data[, "num_sal_var16_ult1"] - 0.00493) * (full_data[, "num_ent_var16_ult1"] - 0.18796),
                    imp_ent_var16_ult1ANDimp_ent_var16_ult1 = (full_data[, "imp_ent_var16_ult1"] - 86.2083) * (full_data[, "imp_ent_var16_ult1"] - 86.2083),
                    imp_ent_var16_ult1ANDnum_ent_var16_ult1 = (full_data[, "imp_ent_var16_ult1"] - 86.2083) * (full_data[, "num_ent_var16_ult1"] - 0.18796),
                    num_ent_var16_ult1ANDnum_ent_var16_ult1 = (full_data[, "num_ent_var16_ult1"] - 0.18796) * (full_data[, "num_ent_var16_ult1"] - 0.18796)
                    )


# Variable 18



# Variable 19



# Variable 20

fixed_data <- cbind(fixed_data,
                    num_var20_0 = full_data[, c("num_var20_0")]
                    )


# Variable 21

fixed_data <- cbind(fixed_data,
                    var21 = full_data[, c("var21")],
                    var21ANDvar21 = (full_data[, c("var21")] - 32.5493) * (full_data[, c("var21")] - 32.5493)
                    )


# Variable 22

fixed_data <- cbind(fixed_data,
                    full_data[, c("num_var22_hace2", "num_var22_ult1", "num_var22_ult3", "num_med_var22_ult3")]
                    )

# Variable 23




# Variable 24

fixed_data <- cbind(fixed_data,
                    saldo_var24 = full_data[, c("saldo_var24")]
                    )


# Variable 25

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var25_cte", "num_var25_0", "saldo_var25")],
                    num_var25_0ANDsaldo_var25 = (full_data[, "num_var25_0"] - 0.08516) * (full_data[, "saldo_var25"] - 72.7357)
                    )


# Variable 26

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var26_cte", "ind_var26_0", "num_var26_0", "saldo_var26")],
                    num_var26_0ANDnum_var26_0 = (full_data[, "num_var26_0"] - 0.08938) * (full_data[, "num_var26_0"] - 0.08938),
                    num_var26_0ANDsaldo_var26 = (full_data[, "num_var26_0"] - 0.08938) * (full_data[, "saldo_var26"]- 76.0816),
                    saldo_var26ANDsaldo_var26 = (full_data[, "saldo_var26"]- 76.0816) * (full_data[, "saldo_var26"]- 76.0816)
                    )



# Variable 27



# Variable 28



# Variable 29



# Variable 30

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var30_0", "ind_var30", "saldo_var30")]
                    )


# Variable 31

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var31_0", "ind_var31", "num_var31")],
                    num_var31ANDind_var31 = (full_data[, "num_var31"] - 0.02013) * (full_data[, "ind_var31"] - 0.00367),
                    num_var31ANDnum_var31 = (full_data[, "ind_var31"] - 0.00367) * (full_data[, "ind_var31"] - 0.00367)
                    )


# Variable 32

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var32_cte", "saldo_var32")],
                    num_var32_0ANDnum_var32_0 = (full_data[, "num_var32"] - 0.00422) * (full_data[, "num_var32"] - 0.00422),
                    num_var32_0ANDsaldo_var32 = (full_data[, "num_var32"] - 0.00422) * (full_data[, "saldo_var32"] - 3.34594),
                    ind_var32ANDsaldo_var32 = (full_data[, "ind_var32"] - 0.00108) * (full_data[, "saldo_var32"] - 3.34594),
                    saldo_var32ANDsaldo_var32 = (full_data[, "saldo_var32"] - 3.34594) * (full_data[, "saldo_var32"] - 3.34594)
                    )


# Variable 33

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var33_0", "ind_var33", "num_var33_0", "num_var33", "saldo_var33", "delta_imp_aport_var33_1y3", "saldo_medio_var33_ult1")]
                    )


# Variable 34



# Variable 35

fixed_data <- cbind(fixed_data,
                    num_var35 = full_data[, "num_var35"],
                    num_var35ANDnum_var35 = (full_data[, "num_var35"] - 3.29937) * (full_data[, "num_var35"] - 3.29937),
                    num_var35ANDnum_var35ANDnum_var35 = (full_data[, "num_var35"] - 3.29937) * (full_data[, "num_var35"] - 3.29937) * (full_data[, "num_var35"] - 3.29937)
                    )


# Variable 36

fixed_data <- cbind(fixed_data,
                    var36 = full_data[, "var36"],
                    var36ANDvar36 = (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491),
                    var36ANDvar36ANDvar36 = (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491)
                    )


# Variable 37


fixed_data <- cbind(fixed_data,
                    full_data[, c("saldo_var37", "imp_trans_var37_ult1")],
                    saldo_var37ANDsaldo_var37 = (full_data[, "saldo_var37"] - 36.9072) * (full_data[, "saldo_var37"] - 36.9072),
                    saldo_var37ANDimp_trans_var37_ult1 = (full_data[, "saldo_var37"] - 36.9072) * (full_data[, "imp_trans_var37_ult1"] - 1932.95),
                    imp_trans_var37_ult1ANDimp_trans_var37_ult1 = (full_data[, "imp_trans_var37_ult1"] - 1932.95) * (full_data[, "imp_trans_var37_ult1"] - 1932.95)
                    )


# Variable 38

fixed_data <- cbind(fixed_data,
                    var38 = full_data[, "var38"])


# Variable 39

fixed_data <- cbind(fixed_data,
                    ind_var39_0ANDind_var39_0 = (full_data[, "ind_var39_0"] - 0.88076) * (full_data[, "ind_var39"] - 0.00372),
                    ind_var39_0ANDnum_op_var39_ult1 = (full_data[, "ind_var39_0"] - 0.88076) * (full_data[, "num_op_var39_ult1"] - 2.91586),
                    ind_var39_0ANDimp_op_var39_comer_ult3 = (full_data[, "ind_var39_0"] - 0.88076) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    ind_var39_0ANDimp_op_var39_efect_ult3 = (full_data[, "ind_var39_0"] - 0.88076) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    ind_var39_0ANDnum_meses_var39_vig_ult3 = (full_data[, "ind_var39_0"] - 0.88076) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    ind_var39_0ANDnum_op_var39_efect_ult3 = (full_data[, "ind_var39_0"] - 0.88076) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_var39_0ANDind_var39 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "ind_var39"] - 0.00372),
                    num_var39_0ANDimp_op_var39_comer_ult1 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "imp_op_var39_comer_ult1"] - 72.3631),
                    num_var39_0ANDimp_op_var39_efect_ult1 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "imp_op_var39_efect_ult1"] - 68.6181),
                    num_var39_0ANDimp_op_var39ult1 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "imp_op_var39_ult1"] - 140.403),
                    num_var39_0ANDnum_op_var39_ult1 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_op_var39_ult1"] - 2.91586),
                    num_var39_0ANDnum_op_var39_comer_ult1 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_op_var39_comer_ult1"] - 2.19479),
                    num_var39_0ANDnum_op_var39_efect_ult1 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    num_var39_0ANDnum_op_var39_hace2 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    num_var39_0ANDimp_op_var39_comer_ult3 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    num_var39_0ANDimp_op_var39_efect_ult3 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    num_var39_0ANDnum_op_var39_hace3 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    num_var39_0ANDnum_meses_var39_vig_ult3 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    num_var39_0ANDnum_op_var39_comer_ult3 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    num_var39_0ANDnum_op_var39_efect_ult3 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    ind_var39ANDind_var39 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "ind_var39"] - 0.00372),
                    ind_var39ANDimp_op_var39_comer_ult1 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "imp_op_var39_comer_ult1"] - 72.3631),
                    ind_var39ANDimp_op_var39_efect_ult1 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "imp_op_var39_efect_ult1"] - 68.6181),
                    ind_var39ANDimp_op_var39_ult1 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "imp_op_var39_ult1"] - 140.403),
                    ind_var39ANDnum_op_var39_ult1 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_op_var39_ult1"] - 2.91586),
                    ind_var39ANDnum_op_var39_comer_ult1 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_op_var39_comer_ult1"] - 2.19479),
                    ind_var39ANDnum_op_var39_efect_ult1 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    ind_var39ANDnum_op_var39_hace2 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    ind_var39ANDimp_op_var39_comer_ult3 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    ind_var39ANDimp_op_var39_efect_ult3 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    ind_var39ANDnum_op_var39_hace3 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    ind_var39ANDnum_meses_var39_vig_ult3 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    ind_var39ANDnum_op_var39_comer_ult3 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    ind_var39ANDnum_op_var39_efect_ult3 = (full_data[, "ind_var39"] - 0.00372) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    ind_op_var39_comer_ult1ANDimp_op_var39_comer_ult1 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "imp_op_var39_comer_ult1"] - 72.3631),
                    ind_op_var39_comer_ult1ANDimp_op_var39_efect_ult1 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "imp_op_var39_efect_ult1"] - 68.6181),
                    ind_op_var39_comer_ult1ANDimp_op_var39_ult1 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "imp_op_var39_ult1"] - 140.403),
                    ind_op_var39_comer_ult1ANDnum_op_var39_ult1 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_op_var39_ult1"] - 2.91586),
                    ind_op_var39_comer_ult1ANDnum_op_var39_comer_ult1 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_op_var39_comer_ult1"] - 2.19479),
                    ind_op_var39_comer_ult1ANDnum_op_var39_efect_ult1 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    ind_op_var39_comer_ult1ANDnum_op_var39_hace2 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    ind_op_var39_comer_ult1ANDimp_op_var39_comer_ult3 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    imp_op_var39_comer_ult1ANDimp_op_var39_efect_ult3 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    imp_op_var39_comer_ult1ANDnum_op_var39_hace3 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    imp_op_var39_comer_ult1ANDnum_meses_var39_vig_ult3 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    imp_op_var39_comer_ult1ANDnum_op_var39_comer_ult3 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    imp_op_var39_comer_ult1ANDnum_op_var39_efect_ult3 = (full_data[, "imp_op_var39_comer_ult1"] - 72.3631) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    imp_op_var39_efect_ult1ANDimp_op_var39_efect_ult1 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "imp_op_var39_efect_ult1"] - 68.6181),
                    imp_op_var39_efect_ult1ANDimp_op_var39_ult1 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "imp_op_var39_ult1"] - 140.403),
                    imp_op_var39_efect_ult1ANDnum_op_var39_ult1 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_op_var39_ult1"] - 2.91586),
                    imp_op_var39_efect_ult1ANDnum_op_var39_comer_ult1 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_op_var39_comer_ult1"] - 2.19479),
                    imp_op_var39_efect_ult1ANDnum_op_var39_efect_ult1 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    imp_op_var39_efect_ult1ANDnum_op_var39_hace2 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    imp_op_var39_efect_ult1ANDimp_op_var39_comer_ult3 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    imp_op_var39_efect_ult1ANDnum_op_var39_hace3 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    imp_op_var39_efect_ult1ANDnum_meses_var39_vig_ult3 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    imp_op_var39_efect_ult1ANDnum_op_var39_comer_ult3 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    imp_op_var39_efect_ult1ANDnum_op_var39_efect_ult3 = (full_data[, "imp_op_var39_efect_ult1"] - 68.6181) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    imp_op_var39_ult1ANDimp_op_var39_ult1 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "imp_op_var39_ult1"] - 140.403),
                    imp_op_var39_ult1ANDnum_op_var39_ult1 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_op_var39_ult1"] - 2.91586),
                    imp_op_var39_ult1ANDnum_op_var39_comer_ult1 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_op_var39_comer_ult1"] - 2.19479),
                    imp_op_var39_ult1ANDnum_op_var39_efect_ult1 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    imp_op_var39_ult1ANDnum_op_var39_hace2 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    imp_op_var39_ult1ANDimp_op_var39_comer_ult3 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    imp_op_var39_ult1ANDimp_op_var39_efect_ult3 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    imp_op_var39_ult1ANDnum_op_var39_hace3 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    imp_op_var39_ult1ANDnum_meses_var39_vig_ult3 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    imp_op_var39_ult1ANDnum_op_var39_comer_ult3 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    imp_op_var39_ult1ANDnum_op_var39_efect_ult3 = (full_data[, "imp_op_var39_ult1"] - 140.403) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_op_var39_ult1ANDnum_op_var39_ult1 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "num_op_var39_ult1"] - 2.91586),
                    num_op_var39_ult1ANDnum_op_var39_comer_ult1 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "num_op_var39_comer_ult1"] - 2.19479),
                    num_op_var39_ult1ANDnum_op_var39_efect_ult1 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    num_op_var39_ult1ANDnum_op_var39_hace2 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    num_op_var39_ult1ANDimp_op_var39_comer_ult3 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    num_op_var39_ult1ANDimp_op_var39_efect_ult3 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    num_op_var39_ult1ANDnum_meses_var39_vig_ult3 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    num_op_var39_ult1ANDnum_op_var39_comer_ult3 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    num_op_var39_ult1ANDnum_op_var39_efect_ult3 = (full_data[, "num_op_var39_ult1"] - 2.91586) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_op_var39_comer_ult1ANDnum_op_var39_comer_ult1 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "num_op_var39_comer_ult1"] - 2.19479),
                    num_op_var39_comer_ult1ANDnum_op_var39_efect_ult1 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    num_op_var39_comer_ult1ANDnum_op_var39_hace2 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    num_op_var39_comer_ult1ANDimp_op_var39_comer_ult3 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    num_op_var39_comer_ult1ANDimp_op_var39_efect_ult3 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    num_op_var39_comer_ult1ANDnum_op_var39_hace3 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    num_op_var39_comer_ult1ANDnum_meses_var39_vig_ult3 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    num_op_var39_comer_ult1ANDnum_op_var39_comer_ult3 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    num_op_var39_comer_ult1ANDnum_op_var39_efect_ult3 = (full_data[, "num_op_var39_comer_ult1"] - 2.19479) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_op_var39_efect_ult1ANDnum_op_var39_efect_ult1 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "num_op_var39_efect_ult1"] - 0.7219),
                    num_op_var39_efect_ult1ANDnum_op_var39_hace2 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    num_op_var39_efect_ult1ANDimp_op_var39_comer_ult3 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    num_op_var39_efect_ult1ANDimp_op_var39_efect_ult3 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    num_op_var39_efect_ult1ANDnum_op_var39_hace3 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    num_op_var39_efect_ult1ANDnum_meses_var39_vig_ult3 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    num_op_var39_efect_ult1ANDnum_op_var39_comer_ult3 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    num_op_var39_efect_ult1ANDnum_op_var39_efect_ult3 = (full_data[, "num_op_var39_efect_ult1"] - 0.7219) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_op_var39_hace2ANDnum_op_var39_hace2 = (full_data[, "num_op_var39_hace2"] - 1.62139) * (full_data[, "num_op_var39_hace2"] - 1.62139),
                    num_op_var39_hace2ANDimp_op_var39_comer_ult3 = (full_data[, "num_op_var39_hace2"] - 1.62139) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    num_op_var39_hace2ANDimp_op_var39_efect_ult3 = (full_data[, "num_op_var39_hace2"] - 1.62139) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    num_op_var39_hace2ANDnum_op_var39_hace3 = (full_data[, "num_op_var39_hace2"] - 1.62139) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    num_op_var39_hace2ANDnum_meses_var39_vig_ult3 = (full_data[, "num_op_var39_hace2"] - 1.62139) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    num_op_var39_hace2ANDnum_op_var39_comer_ult3 = (full_data[, "num_op_var39_hace2"] - 1.62139) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    num_op_var39_hace2ANDnum_op_var39_efect_ult3 = (full_data[, "num_op_var39_hace2"] - 1.62139) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    imp_op_var39_comer_ult3ANDimp_op_var39_comer_ult3 = (full_data[, "imp_op_var39_comer_ult3"] - 119.53) * (full_data[, "imp_op_var39_comer_ult3"] - 119.53),
                    imp_op_var39_comer_ult3ANDimp_op_var39_efect_ult3 = (full_data[, "imp_op_var39_comer_ult3"] - 119.53) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    imp_op_var39_comer_ult3ANDnum_op_var39_hace3 = (full_data[, "imp_op_var39_comer_ult3"] - 119.53) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    imp_op_var39_comer_ult3ANDnum_meses_var39_vig_ult3 = (full_data[, "imp_op_var39_comer_ult3"] - 119.53) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    imp_op_var39_comer_ult3ANDnum_op_var39_comer_ult3 = (full_data[, "imp_op_var39_comer_ult3"] - 119.53) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    imp_op_var39_comer_ult3ANDnum_op_var39_efect_ult3 = (full_data[, "imp_op_var39_comer_ult3"] - 119.53) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    imp_op_var39_efect_ult3ANDimp_op_var39_efect_ult3 = (full_data[, "imp_op_var39_efect_ult3"] - 113.792) * (full_data[, "imp_op_var39_efect_ult3"] - 113.792),
                    imp_op_var39_efect_ult3ANDnum_op_var39_hace3 = (full_data[, "imp_op_var39_efect_ult3"] - 113.792) * (full_data[, "num_op_var39_hace3"] - 0.09495),
                    imp_op_var39_efect_ult3ANDnum_meses_var39_vig_ult3 = (full_data[, "imp_op_var39_efect_ult3"] - 113.792) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    imp_op_var39_efect_ult3ANDnum_op_var39_comer_ult3 = (full_data[, "imp_op_var39_efect_ult3"] - 113.792) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    imp_op_var39_efect_ult3ANDnum_op_var39_efect_ult3 = (full_data[, "imp_op_var39_efect_ult3"] - 113.792) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_op_var39_hace3ANDnum_meses_var39_vig_ult3 = (full_data[, "num_op_var39_hace3"] - 0.09495) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    num_meses_var39_vig_ult3ANDnum_meses_var39_vig_ult3 = (full_data[, "num_meses_var39_vig_ult3"] - 1.59279) * (full_data[, "num_meses_var39_vig_ult3"] - 1.59279),
                    num_meses_var39_vig_ult3ANDnum_op_var39_comer_ult3 = (full_data[, "num_meses_var39_vig_ult3"] - 1.59279) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    num_meses_var39_vig_ult3ANDnum_op_var39_efect_ult3 = (full_data[, "num_meses_var39_vig_ult3"] - 1.59279) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_op_var39_comer_ult3ANDnum_op_var39_comer_ult3 = (full_data[, "num_op_var39_comer_ult3"] - 3.60706) * (full_data[, "num_op_var39_comer_ult3"] - 3.60706),
                    num_op_var39_comer_ult3ANDnum_op_var39_efect_ult3 = (full_data[, "num_op_var39_comer_ult3"] - 3.60706) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582),
                    num_op_var39_efect_ult3ANDnum_op_var39_efect_ult3 = (full_data[, "num_op_var39_efect_ult3"] - 1.21582) * (full_data[, "num_op_var39_efect_ult3"] - 1.21582)
                    )


# Variable 40



# Variable 41

fixed_data <- cbind(fixed_data,
                    full_data[, c("num_var41_0", "imp_op_var41_comer_ult1", "imp_op_var41_efect_ult1", "imp_op_var41_ult1", "num_op_var41_efect_ult1")],
                    num_var41_0ANDimp_op_var41_comer_ult1 = (full_data[, "num_var41_0"] - 2.69925) * (full_data[, "imp_op_var41_comer_ult1"] - 68.8039),
                    num_var41_0ANDimp_op_var41_ult1 = (full_data[, "num_var41_0"] - 2.69925) * (full_data[, "imp_op_var41_ult1"] - 137.243),
                    imp_op_var41_comer_ult1ANDnum_op_var41_efect_ult1 = (full_data[, "imp_op_var41_comer_ult1"] - 68.8039) * (full_data[, "num_op_var41_efect_ult1"] - 0.71942),
                    imp_op_var41_efect_ult1ANDimp_op_var41_efect_ult1 = (full_data[, "imp_op_var41_efect_ult1"] - 68.2051) * (full_data[, "imp_op_var41_efect_ult1"] - 68.2051)
                    )


# Variable 42

fixed_data <- cbind(fixed_data,
                    full_data[, c("num_var42_0", "num_var42", "saldo_var42")],
                    num_var42_0ANDnum_var42_0 = (full_data[, "num_var42_0"] - 3.20414) * (full_data[, "num_var42_0"] - 3.20414),
                    num_var42_0ANDnum_var42 = (full_data[, "num_var42_0"] - 3.20414) * (full_data[, "num_var42"] - 2.218),
                    saldo_var42ANDsaldo_var42 = (full_data[, "saldo_var42"] - 7191.73) * (full_data[, "saldo_var42"] - 7191.73),
                    saldo_var42ANDsaldo_var42ANDsaldo_var42 = (full_data[, "saldo_var42"] - 7191.73) * (full_data[, "saldo_var42"] - 7191.73) * (full_data[, "saldo_var42"] - 7191.73)
                    )


# Variable 43

fixed_data <- cbind(fixed_data,
                    full_data[, c("ind_var43_emit_ult1", "num_var43_emit_ult1", "ind_var43_recib_ult1", "num_var43_recib_ult1")]
                    )


# Variable 44

fixed_data <- cbind(fixed_data,
                    num_meses_var44_ult3 = full_data[, "num_meses_var44_ult3"]
                    )


# Variable 45

fixed_data <- cbind(fixed_data,
                    full_data[, c("num_med_var45_ult3", "num_var45_hace2", "num_var45_hace3", "num_var45_ult1")],
                    num_med_var45_ult3ANDnum_var45_ult1 = (full_data[, "num_med_var45_ult3"] - 4.02466) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_hace2ANDnum_var45_hace2 = (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_hace2"] - 5.39321),
                    num_var45_hace2ANDnum_var45_ult1 = (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_hace3ANDnum_var45_hace3 = (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_hace3"] - 3.8944),
                    num_var45_hace3ANDnum_var45_ult1 = (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_ult1ANDnum_var45_ult1 = (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_med_var45_ult3ANDnum_med_var45_ult3 = (full_data[, "num_med_var45_ult3"] - 4.02466) * (full_data[, "num_med_var45_ult3"] - 4.02466),
                    num_med_var45_ult3ANDnum_var45_hace2 = (full_data[, "num_med_var45_ult3"] - 4.02466) * (full_data[, "num_var45_hace2"] - 5.39321),
                    num_med_var45_ult3ANDnum_var45_hace3 = (full_data[, "num_med_var45_ult3"] - 4.02466) * (full_data[, "num_var45_hace3"] - 3.8944),
                    num_med_var45_ult3ANDnum_var45_ult1ANDnum_med_var45_ult3 = (full_data[, "num_med_var45_ult3"] - 4.02466) * (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_med_var45_ult3"] - 4.02466),
                    num_med_var45_ult3ANDnum_var45_ult1ANDnum_var45_hace3 = (full_data[, "num_med_var45_ult3"] - 4.02466) * (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_var45_hace3"] - 3.8944),
                    num_med_var45_ult3ANDnum_var45_ult1ANDnum_var45_ult1 = (full_data[, "num_med_var45_ult3"] - 4.02466) * (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_hace2ANDnum_var45_hace2ANDnum_med_var45_ult3 = (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_med_var45_ult3"] - 4.02466),
                    num_var45_hace2ANDnum_var45_hace2ANDnum_var45_hace2 = (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_hace2"] - 5.39321),
                    num_var45_hace2ANDnum_var45_hace2ANDnum_var45_ult1 = (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_hace2ANDnum_var45_ult1ANDnum_var45_ult1 = (full_data[, "num_var45_hace2"] - 5.39321) * (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_hace3ANDnum_var45_hace3ANDnum_med_var45_ult3 = (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_med_var45_ult3"] - 4.02466),
                    num_var45_hace3ANDnum_var45_hace3ANDnum_var45_hace3 = (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_hace3"] - 3.8944),
                    num_var45_hace3ANDnum_var45_hace3ANDnum_var45_ult1 = (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_hace3ANDnum_var45_ult1ANDnum_var45_ult1 = (full_data[, "num_var45_hace3"] - 3.8944) * (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_var45_ult1"] - 4.3635),
                    num_var45_ult1ANDnum_var45_ult1ANDnum_var45_ult1 = (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_var45_ult1"] - 4.3635) * (full_data[, "num_var45_ult1"] - 4.3635)
                    )


# Variable 46



# Output to CSV


write.csv(full_data, file = "fulldata_parsed.csv", row.names = FALSE)
