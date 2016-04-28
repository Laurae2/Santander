feature_creation <- function(full_data) {
  
  
  fixed_data <- data.frame(target = full_data[, "target"])
  
  # Variable 1
  
  cat("Created feature 1\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("saldo_var1", "num_var1_0", "num_var1")],
                      saldo_var1ANDsaldo_var1 = (full_data[, "saldo_var1"] - 48.4491) * (full_data[, "saldo_var1"]), 
                      num_var1_0ANDnum_var1_0 = (full_data[, "num_var1_0"] - 0.03445) * (full_data[, "num_var1_0"] - 0.03445)
  )
  
  
  # Variable 2
  
  
  
  # Variable 3
  
  cat("Created feature 3\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      var3 = full_data[, "var3"],
                      var3ANDvar3 = (full_data[, "var3"] + 1523.2) * (full_data[, "var3"] + 1523.2)
  )
  
  
  # Variable 4
  
  cat("Created feature 4\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      num_var4 = full_data[, "num_var4"],
                      num_var4ANDnum_var4 = (full_data[, "num_var4"] - 1.07944) * (full_data[, "num_var4"] - 1.07944),
                      num_var4ANDnum_var4ANDnum_var4 = (full_data[, "num_var4"] - 1.07944) * (full_data[, "num_var4"] - 1.07944) * (full_data[, "num_var4"] - 1.07944)
  )
  
  
  # Variable 5
  
  cat("Created feature 5\n", sep = "")
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
  
  cat("Created feature 8\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("num_var8_0", "num_var8", "saldo_var8", "saldo_medio_var8_hace2")]
  )
  
  
  # Variable 9
  
  cat("Created feature 9\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var9_cte_ult1", "ind_var9_ult1")]
  )
  
  
  # Variable 10
  
  cat("Created feature 10\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var10_ult1", "ind_var10cte_ult1")]
  )
  
  
  # Variable 11
  
  cat("Created feature 11\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      num_trasp_var11_ult1 = full_data[, c("num_trasp_var11_ult1")]
  )
  
  
  # Variable 12
  
  cat("Created feature 12\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var12_0", "num_var12", "saldo_var12", "num_meses_var12_ult3")]
  )
  
  
  # Variable 13
  
  cat("Created feature 13\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var13_0", "num_aport_var13_hace3")]
  )
  
  
  # Variable 14
  
  cat("Created feature 14\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var14_0", "ind_var14", "saldo_var14")]
  )
  
  
  # Variable 15
  
  cat("Created feature 15\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      var15 = full_data[, c("var15")],
                      var15ANDvar15 = (full_data[, c("var15")] - 33.2129) * (full_data[, c("var15")] - 33.2129),
                      var15ANDvar15ANDvar15 = (full_data[, c("var15")] - 33.2129) * (full_data[, c("var15")] - 33.2129) * (full_data[, c("var15")] - 33.2129)
  )
  
  
  # Variable 16
  
  cat("Created feature 16\n", sep = "")
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
  
  
  # Variable 17
  
  cat("Created feature 17\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var17", "num_meses_var17_ult3")]
  )
  
  
  # Variable 18
  
  
  
  # Variable 19
  
  
  
  # Variable 20
  
  cat("Created feature 20\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      num_var20_0 = full_data[, c("num_var20_0")]
  )
  
  
  # Variable 21
  
  cat("Created feature 21\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      var21 = full_data[, c("var21")],
                      var21ANDvar21 = (full_data[, c("var21")] - 32.5493) * (full_data[, c("var21")] - 32.5493)
  )
  
  
  # Variable 22
  
  cat("Created feature 22\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("num_var22_hace2", "num_var22_ult1", "num_var22_ult3", "num_med_var22_ult3")]
  )
  
  # Variable 23
  
  
  
  
  # Variable 24
  
  cat("Created feature 24\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      saldo_var24 = full_data[, c("saldo_var24")]
  )
  
  
  # Variable 25
  
  cat("Created feature 25\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var25_cte", "num_var25_0", "saldo_var25")],
                      num_var25_0ANDsaldo_var25 = (full_data[, "num_var25_0"] - 0.08516) * (full_data[, "saldo_var25"] - 72.7357)
  )
  
  
  # Variable 26
  
  cat("Created feature 26\n", sep = "")
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
  
  cat("Created feature 30\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var30_0", "ind_var30", "saldo_var30")]
  )
  
  
  # Variable 31
  
  cat("Created feature 31\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var31_0", "ind_var31", "num_var31")],
                      num_var31ANDind_var31 = (full_data[, "num_var31"] - 0.02013) * (full_data[, "ind_var31"] - 0.00367),
                      num_var31ANDnum_var31 = (full_data[, "ind_var31"] - 0.00367) * (full_data[, "num_var31"] - 0.01606)
  )
  
  
  # Variable 32
  
  cat("Created feature 32\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var32_cte", "saldo_var32")],
                      num_var32_0ANDnum_var32_0 = (full_data[, "num_var32"] - 0.00422) * (full_data[, "num_var32"] - 0.00422),
                      num_var32_0ANDsaldo_var32 = (full_data[, "num_var32"] - 0.00422) * (full_data[, "saldo_var32"] - 3.34594),
                      ind_var32ANDsaldo_var32 = (full_data[, "ind_var32"] - 0.00108) * (full_data[, "saldo_var32"] - 3.34594),
                      saldo_var32ANDsaldo_var32 = (full_data[, "saldo_var32"] - 3.34594) * (full_data[, "saldo_var32"] - 3.34594)
  )
  
  
  # Variable 33
  
  cat("Created feature 33\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var33_0", "ind_var33", "num_var33_0", "num_var33", "saldo_var33", "delta_imp_aport_var33_1y3", "saldo_medio_var33_ult1")]
  )
  
  
  # Variable 34
  
  
  
  # Variable 35
  
  cat("Created feature 35\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      num_var35 = full_data[, "num_var35"],
                      num_var35ANDnum_var35 = (full_data[, "num_var35"] - 3.29937) * (full_data[, "num_var35"] - 3.29937),
                      num_var35ANDnum_var35ANDnum_var35 = (full_data[, "num_var35"] - 3.29937) * (full_data[, "num_var35"] - 3.29937) * (full_data[, "num_var35"] - 3.29937)
  )
  
  
  # Variable 36
  
  cat("Created feature 36\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      var36 = full_data[, "var36"],
                      var36ANDvar36 = (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491),
                      var36ANDvar36ANDvar36 = (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491),
                      var36ANDvar36ANDvar36ANDvar36 = (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491) * (full_data[, "var36"] - 40.4491)
  )
  
  
  # Variable 37
  
  cat("Created feature 37\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("saldo_var37", "imp_trans_var37_ult1")],
                      saldo_var37ANDsaldo_var37 = (full_data[, "saldo_var37"] - 36.9072) * (full_data[, "saldo_var37"] - 36.9072),
                      saldo_var37ANDimp_trans_var37_ult1 = (full_data[, "saldo_var37"] - 36.9072) * (full_data[, "imp_trans_var37_ult1"] - 1932.95),
                      imp_trans_var37_ult1ANDimp_trans_var37_ult1 = (full_data[, "imp_trans_var37_ult1"] - 1932.95) * (full_data[, "imp_trans_var37_ult1"] - 1932.95)
  )
  
  
  # Variable 38
  
  cat("Created feature 38\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      var38 = full_data[, "var38"])
  
  
  # Variable 39
  
  cat("Created feature 39\n", sep = "")
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
                      num_var39_0ANDimp_op_var39_ult1 = (full_data[, "num_var39_0"] - 2.72494) * (full_data[, "imp_op_var39_ult1"] - 140.403),
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
  
  cat("Created feature 41\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("num_var41_0", "imp_op_var41_comer_ult1", "imp_op_var41_efect_ult1", "imp_op_var41_ult1", "num_op_var41_efect_ult1")],
                      num_var41_0ANDimp_op_var41_comer_ult1 = (full_data[, "num_var41_0"] - 2.69925) * (full_data[, "imp_op_var41_comer_ult1"] - 68.8039),
                      num_var41_0ANDimp_op_var41_ult1 = (full_data[, "num_var41_0"] - 2.69925) * (full_data[, "imp_op_var41_ult1"] - 137.243),
                      imp_op_var41_comer_ult1ANDnum_op_var41_efect_ult1 = (full_data[, "imp_op_var41_comer_ult1"] - 68.8039) * (full_data[, "num_op_var41_efect_ult1"] - 0.71942),
                      imp_op_var41_efect_ult1ANDimp_op_var41_efect_ult1 = (full_data[, "imp_op_var41_efect_ult1"] - 68.2051) * (full_data[, "imp_op_var41_efect_ult1"] - 68.2051)
  )
  
  
  # Variable 42
  
  cat("Created feature 42\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("num_var42_0", "num_var42", "saldo_var42")],
                      num_var42_0ANDnum_var42_0 = (full_data[, "num_var42_0"] - 3.20414) * (full_data[, "num_var42_0"] - 3.20414),
                      num_var42_0ANDnum_var42 = (full_data[, "num_var42_0"] - 3.20414) * (full_data[, "num_var42"] - 2.218),
                      saldo_var42ANDsaldo_var42 = (full_data[, "saldo_var42"] - 7191.73) * (full_data[, "saldo_var42"] - 7191.73),
                      saldo_var42ANDsaldo_var42ANDsaldo_var42 = (full_data[, "saldo_var42"] - 7191.73) * (full_data[, "saldo_var42"] - 7191.73) * (full_data[, "saldo_var42"] - 7191.73)
  )
  
  
  # Variable 43
  
  cat("Created feature 43\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      full_data[, c("ind_var43_emit_ult1", "num_var43_emit_ult1", "ind_var43_recib_ult1", "num_var43_recib_ult1")]
  )
  
  
  # Variable 44
  
  cat("Created feature 44\n", sep = "")
  fixed_data <- cbind(fixed_data,
                      num_meses_var44_ult3 = full_data[, "num_meses_var44_ult3"]
  )
  
  
  # Variable 45
  
  cat("Created feature 45\n", sep = "")
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
  
  
  
  return(fixed_data)
}


linear_interactions <- function(fixed_data, train_target, train) {
  
  fixed_data_features <- fixed_data
  
  
  # Variable 1
  temporary <- glm(target ~ saldo_var1 + num_var1_0 + num_var1 + saldo_var1ANDsaldo_var1 + num_var1_0ANDnum_var1_0, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var1GLM = temporary_out)
  cat("Variable 1 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 3
  temporary <- glm(target ~ var3 + var3ANDvar3, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var3GLM = temporary_out)
  cat("Variable 3 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 4
  temporary <- glm(target ~ num_var4 + num_var4ANDnum_var4 + num_var4ANDnum_var4ANDnum_var4, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var4GLM = temporary_out)
  cat("Variable 4 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 5
  temporary <- glm(target ~ num_var5_0 + num_var5 + saldo_var5 + num_meses_var5_ult3 + saldo_medio_var5_hace2 + saldo_medio_var5_hace3 + saldo_medio_var5_ult1 + saldo_medio_var5_ult3
                   + num_var5_0ANDnum_var5_0 + num_var5_0ANDnum_var5 + num_var5_0ANDsaldo_var5 + num_var5_0ANDnum_meses_var5_ult3 + num_var5_0ANDsaldo_medio_var5_hace2 + num_var5_0ANDsaldo_medio_var5_ult1 + num_var5_0ANDsaldo_medio_var5_ult3
                   + num_var5ANDnum_var5 + num_var5ANDsaldo_var5 + num_var5ANDnum_meses_var5_ult3 + num_var5ANDsaldo_medio_var5_hace2 + num_var5ANDsaldo_medio_var5_hace3 + num_var5ANDsaldo_medio_var5_ult1 + num_var5ANDsaldo_medio_var5_ult3
                   + saldo_var5ANDsaldo_var5 + saldo_var5ANDnum_meses_var5_ult3 + saldo_var5ANDsaldo_medio_var5_hace2 + saldo_var5ANDsaldo_medio_var5_hace3 + saldo_var5ANDsaldo_medio_var5_ult1 + saldo_var5ANDsaldo_medio_var5_ult3
                   + num_meses_var5_ult3ANDnum_meses_var5_ult3 + num_meses_var5_ult3ANDsaldo_medio_var5_hace3 + num_meses_var5_ult3ANDsaldo_medio_var5_ult1 + num_meses_var5_ult3ANDsaldo_medio_var5_ult3
                   + saldo_medio_var5_hace2ANDsaldo_medio_var5_hace2 + saldo_medio_var5_hace2ANDsaldo_medio_var5_ult1 + saldo_medio_var5_hace2ANDsaldo_medio_var5_ult3
                   + saldo_medio_var5_hace3ANDsaldo_medio_var5_hace3 + saldo_medio_var5_hace3ANDsaldo_medio_var5_ult1 + saldo_medio_var5_hace3ANDsaldo_medio_var5_ult3
                   + saldo_medio_var5_ult1ANDsaldo_medio_var5_ult1 + saldo_medio_var5_ult1ANDsaldo_medio_var5_ult3 + saldo_medio_var5_ult3ANDsaldo_medio_var5_ult3,
                   family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var5GLM = temporary_out)
  cat("Variable 5 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 8
  temporary <- glm(target ~ num_var8_0 + num_var8 + saldo_var8 + saldo_medio_var8_hace2, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var8GLM = temporary_out)
  cat("Variable 8 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 9
  temporary <- glm(target ~ ind_var9_cte_ult1 + ind_var9_ult1, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var9GLM = temporary_out)
  cat("Variable 9 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 10
  temporary <- glm(target ~ ind_var10_ult1 + ind_var10cte_ult1, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var10GLM = temporary_out)
  cat("Variable 10 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 11
  temporary <- glm(target ~ num_trasp_var11_ult1, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var11GLM = temporary_out)
  cat("Variable 11 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 12
  temporary <- glm(target ~ ind_var12_0 + num_var12 + saldo_var12 + num_meses_var12_ult3, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var12GLM = temporary_out)
  cat("Variable 12 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 13
  temporary <- glm(target ~ ind_var13_0 + num_aport_var13_hace3, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var13GLM = temporary_out)
  cat("Variable 13 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 14
  temporary <- glm(target ~ ind_var14_0 + ind_var14 + saldo_var14, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var14GLM = temporary_out)
  cat("Variable 14 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 15
  temporary <- glm(target ~ var15 + var15ANDvar15 + var15ANDvar15ANDvar15, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var15GLM = temporary_out)
  cat("Variable 15 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 16
  temporary <- glm(target ~ imp_sal_var16_ult1 + num_sal_var16_ult1 + imp_ent_var16_ult1 + num_ent_var16_ult1
                   + imp_sal_var16_ult1ANDimp_sal_var16_ult1 + imp_sal_var16_ult1ANDnum_sal_var16_ult1 + imp_sal_var16_ult1ANDimp_ent_var16_ult1 + imp_sal_var16_ult1ANDnum_ent_var16_ult1
                   + num_sal_var16_ult1ANDnum_sal_var16_ult1 + num_sal_var16_ult1ANDimp_ent_var16_ult1 + num_sal_var16_ult1ANDnum_ent_var16_ult1
                   + imp_ent_var16_ult1ANDimp_ent_var16_ult1 + imp_ent_var16_ult1ANDnum_ent_var16_ult1
                   + num_ent_var16_ult1ANDnum_ent_var16_ult1,
                   family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var16GLM = temporary_out)
  cat("Variable 16 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 17
  temporary <- glm(target ~ ind_var17 + num_meses_var17_ult3, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var17GLM = temporary_out)
  cat("Variable 17 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 20
  temporary <- glm(target ~ num_var20_0, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var20GLM = temporary_out)
  cat("Variable 20 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 21
  temporary <- glm(target ~ var21 + var21ANDvar21, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var21GLM = temporary_out)
  cat("Variable 21 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 22
  temporary <- glm(target ~ num_var22_hace2 + num_var22_ult1 + num_var22_ult3 + num_med_var22_ult3, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var22GLM = temporary_out)
  cat("Variable 22 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 24
  temporary <- glm(target ~ saldo_var24, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var24GLM = temporary_out)
  cat("Variable 24 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 25
  temporary <- glm(target ~ ind_var25_cte + num_var25_0 + saldo_var25 + num_var25_0ANDsaldo_var25 , family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var25GLM = temporary_out)
  cat("Variable 25 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 26
  temporary <- glm(target ~ ind_var26_cte + ind_var26_0 + num_var26_0 + saldo_var26 + num_var26_0ANDnum_var26_0 + num_var26_0ANDsaldo_var26 + saldo_var26ANDsaldo_var26, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var26GLM = temporary_out)
  cat("Variable 26 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 30
  temporary <- glm(target ~ ind_var30_0 + ind_var30 + saldo_var30, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var30GLM = temporary_out)
  cat("Variable 30 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 31
  temporary <- glm(target ~ ind_var31_0 + ind_var31 + num_var31 + num_var31ANDind_var31 + num_var31ANDnum_var31, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var31GLM = temporary_out)
  cat("Variable 31 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 32
  temporary <- glm(target ~ ind_var32_cte + saldo_var32 + num_var32_0ANDnum_var32_0 + num_var32_0ANDsaldo_var32 + ind_var32ANDsaldo_var32 + saldo_var32ANDsaldo_var32, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var32GLM = temporary_out)
  cat("Variable 32 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 33
  temporary <- glm(target ~ ind_var33_0 +ind_var33 + num_var33_0 + num_var33 + saldo_var33 + delta_imp_aport_var33_1y3 + saldo_medio_var33_ult1, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var33GLM = temporary_out)
  cat("Variable 33 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 35
  temporary <- glm(target ~ num_var35 + num_var35ANDnum_var35 + num_var35ANDnum_var35ANDnum_var35, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var35GLM = temporary_out)
  cat("Variable 35 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 36
  temporary <- glm(target ~ var36 + var36ANDvar36 + var36ANDvar36ANDvar36 + var36ANDvar36ANDvar36ANDvar36, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var36GLM = temporary_out)
  cat("Variable 36 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 37
  temporary <- glm(target ~ saldo_var37 + imp_trans_var37_ult1 + saldo_var37ANDsaldo_var37 + saldo_var37ANDimp_trans_var37_ult1 + imp_trans_var37_ult1ANDimp_trans_var37_ult1, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var37GLM = temporary_out)
  cat("Variable 37 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 38
  temporary <- glm(target ~ var38, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var38GLM = temporary_out)
  cat("Variable 38 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 39
  temporary <- glm(target ~
                     ind_var39_0ANDind_var39_0 + ind_var39_0ANDnum_op_var39_ult1 + ind_var39_0ANDimp_op_var39_comer_ult3 + ind_var39_0ANDimp_op_var39_efect_ult3 + ind_var39_0ANDnum_meses_var39_vig_ult3 + ind_var39_0ANDnum_op_var39_efect_ult3
                   + num_var39_0ANDind_var39 + num_var39_0ANDimp_op_var39_comer_ult1 + num_var39_0ANDimp_op_var39_efect_ult1 + num_var39_0ANDimp_op_var39_ult1 + num_var39_0ANDnum_op_var39_ult1 + num_var39_0ANDnum_op_var39_comer_ult1 + num_var39_0ANDnum_op_var39_efect_ult1 + num_var39_0ANDnum_op_var39_hace2 + num_var39_0ANDimp_op_var39_comer_ult3 + num_var39_0ANDimp_op_var39_efect_ult3 + num_var39_0ANDnum_op_var39_hace3 + num_var39_0ANDnum_meses_var39_vig_ult3 + num_var39_0ANDnum_op_var39_comer_ult3 + num_var39_0ANDnum_op_var39_efect_ult3
                   + ind_var39ANDind_var39 + ind_var39ANDimp_op_var39_comer_ult1 + ind_var39ANDimp_op_var39_efect_ult1 + ind_var39ANDimp_op_var39_ult1 + ind_var39ANDnum_op_var39_ult1 + ind_var39ANDnum_op_var39_comer_ult1 + ind_var39ANDnum_op_var39_efect_ult1 + ind_var39ANDnum_op_var39_hace2 + ind_var39ANDimp_op_var39_comer_ult3 + ind_var39ANDimp_op_var39_efect_ult3 + ind_var39ANDnum_op_var39_hace3 + ind_var39ANDnum_meses_var39_vig_ult3 + ind_var39ANDnum_op_var39_comer_ult3 + ind_var39ANDnum_op_var39_efect_ult3
                   + ind_op_var39_comer_ult1ANDimp_op_var39_comer_ult1 + ind_op_var39_comer_ult1ANDimp_op_var39_efect_ult1 + ind_op_var39_comer_ult1ANDimp_op_var39_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_comer_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_efect_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_hace2 + ind_op_var39_comer_ult1ANDimp_op_var39_comer_ult3 + imp_op_var39_comer_ult1ANDimp_op_var39_efect_ult3 + imp_op_var39_comer_ult1ANDnum_op_var39_hace3 + imp_op_var39_comer_ult1ANDnum_meses_var39_vig_ult3 + imp_op_var39_comer_ult1ANDnum_op_var39_comer_ult3 + imp_op_var39_comer_ult1ANDnum_op_var39_efect_ult3
                   + imp_op_var39_efect_ult1ANDimp_op_var39_efect_ult1 + imp_op_var39_efect_ult1ANDimp_op_var39_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_comer_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_efect_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_hace2 + imp_op_var39_efect_ult1ANDimp_op_var39_comer_ult3 + imp_op_var39_efect_ult1ANDnum_op_var39_hace3 + imp_op_var39_efect_ult1ANDnum_meses_var39_vig_ult3 + imp_op_var39_efect_ult1ANDnum_op_var39_comer_ult3 + imp_op_var39_efect_ult1ANDnum_op_var39_efect_ult3
                   + imp_op_var39_ult1ANDimp_op_var39_ult1 + imp_op_var39_ult1ANDnum_op_var39_ult1 + imp_op_var39_ult1ANDnum_op_var39_comer_ult1 + imp_op_var39_ult1ANDnum_op_var39_efect_ult1 + imp_op_var39_ult1ANDnum_op_var39_hace2 + imp_op_var39_ult1ANDimp_op_var39_comer_ult3 + imp_op_var39_ult1ANDimp_op_var39_efect_ult3 + imp_op_var39_ult1ANDnum_op_var39_hace3 + imp_op_var39_ult1ANDnum_meses_var39_vig_ult3 + imp_op_var39_ult1ANDnum_op_var39_comer_ult3 + imp_op_var39_ult1ANDnum_op_var39_efect_ult3
                   + num_op_var39_ult1ANDnum_op_var39_ult1 + num_op_var39_ult1ANDnum_op_var39_comer_ult1 + num_op_var39_ult1ANDnum_op_var39_efect_ult1 + num_op_var39_ult1ANDnum_op_var39_hace2 + num_op_var39_ult1ANDimp_op_var39_comer_ult3 + num_op_var39_ult1ANDimp_op_var39_efect_ult3 + num_op_var39_ult1ANDnum_meses_var39_vig_ult3 + num_op_var39_ult1ANDnum_op_var39_comer_ult3 + num_op_var39_ult1ANDnum_op_var39_efect_ult3
                   + num_op_var39_comer_ult1ANDnum_op_var39_comer_ult1 + num_op_var39_comer_ult1ANDnum_op_var39_efect_ult1 + num_op_var39_comer_ult1ANDnum_op_var39_hace2 + num_op_var39_comer_ult1ANDimp_op_var39_comer_ult3 + num_op_var39_comer_ult1ANDimp_op_var39_efect_ult3 + num_op_var39_comer_ult1ANDnum_op_var39_hace3 + num_op_var39_comer_ult1ANDnum_meses_var39_vig_ult3 + num_op_var39_comer_ult1ANDnum_op_var39_comer_ult3 + num_op_var39_comer_ult1ANDnum_op_var39_efect_ult3
                   + num_op_var39_efect_ult1ANDnum_op_var39_efect_ult1 + num_op_var39_efect_ult1ANDnum_op_var39_hace2 + num_op_var39_efect_ult1ANDimp_op_var39_comer_ult3 + num_op_var39_efect_ult1ANDimp_op_var39_efect_ult3 + num_op_var39_efect_ult1ANDnum_op_var39_hace3 + num_op_var39_efect_ult1ANDnum_meses_var39_vig_ult3 + num_op_var39_efect_ult1ANDnum_op_var39_comer_ult3 + num_op_var39_efect_ult1ANDnum_op_var39_efect_ult3
                   + num_op_var39_hace2ANDnum_op_var39_hace2 + num_op_var39_hace2ANDimp_op_var39_comer_ult3 + num_op_var39_hace2ANDimp_op_var39_efect_ult3 + num_op_var39_hace2ANDnum_op_var39_hace3 + num_op_var39_hace2ANDnum_meses_var39_vig_ult3 + num_op_var39_hace2ANDnum_op_var39_comer_ult3 + num_op_var39_hace2ANDnum_op_var39_efect_ult3
                   + imp_op_var39_comer_ult3ANDimp_op_var39_comer_ult3 + imp_op_var39_comer_ult3ANDimp_op_var39_efect_ult3 + imp_op_var39_comer_ult3ANDnum_op_var39_hace3 + imp_op_var39_comer_ult3ANDnum_meses_var39_vig_ult3 + imp_op_var39_comer_ult3ANDnum_op_var39_comer_ult3 + imp_op_var39_comer_ult3ANDnum_op_var39_efect_ult3
                   + imp_op_var39_efect_ult3ANDimp_op_var39_efect_ult3 + imp_op_var39_efect_ult3ANDnum_op_var39_hace3 + imp_op_var39_efect_ult3ANDnum_meses_var39_vig_ult3 + imp_op_var39_efect_ult3ANDnum_op_var39_comer_ult3 + imp_op_var39_efect_ult3ANDnum_op_var39_efect_ult3
                   + num_op_var39_hace3ANDnum_meses_var39_vig_ult3
                   + num_meses_var39_vig_ult3ANDnum_meses_var39_vig_ult3 + num_meses_var39_vig_ult3ANDnum_op_var39_comer_ult3 + num_meses_var39_vig_ult3ANDnum_op_var39_efect_ult3
                   + num_op_var39_comer_ult3ANDnum_op_var39_comer_ult3 + num_op_var39_comer_ult3ANDnum_op_var39_efect_ult3 + num_op_var39_efect_ult3ANDnum_op_var39_efect_ult3,
                   family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var39GLM = temporary_out)
  cat("Variable 39 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 41
  temporary <- glm(target ~ num_var41_0 + imp_op_var41_comer_ult1 + imp_op_var41_efect_ult1 + imp_op_var41_ult1 + num_op_var41_efect_ult1
                   + num_var41_0ANDimp_op_var41_comer_ult1 + num_var41_0ANDimp_op_var41_ult1 + imp_op_var41_comer_ult1ANDnum_op_var41_efect_ult1 + imp_op_var41_efect_ult1ANDimp_op_var41_efect_ult1
                   , family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var41GLM = temporary_out)
  cat("Variable 41 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 42
  temporary <- glm(target ~ num_var42_0 + num_var42 + saldo_var42
                   + num_var42_0ANDnum_var42_0 + num_var42_0ANDnum_var42 + saldo_var42ANDsaldo_var42 + saldo_var42ANDsaldo_var42ANDsaldo_var42
                   , family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var42GLM = temporary_out)
  cat("Variable 42 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 43
  temporary <- glm(target ~ ind_var43_emit_ult1 + num_var43_emit_ult1 + ind_var43_recib_ult1 + num_var43_recib_ult1, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var43GLM = temporary_out)
  cat("Variable 43 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 44
  temporary <- glm(target ~ num_meses_var44_ult3, family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var44GLM = temporary_out)
  cat("Variable 44 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  # Variable 45
  temporary <- glm(target ~ num_med_var45_ult3 + num_var45_hace2 + num_var45_hace3 + num_var45_ult1
                   + num_med_var45_ult3ANDnum_var45_ult1
                   + num_var45_hace2ANDnum_var45_hace2 + num_var45_hace2ANDnum_var45_ult1
                   + num_var45_hace3ANDnum_var45_hace3 + num_var45_hace3ANDnum_var45_ult1
                   + num_var45_ult1ANDnum_var45_ult1
                   + num_med_var45_ult3ANDnum_med_var45_ult3 + num_med_var45_ult3ANDnum_var45_hace2 + num_med_var45_ult3ANDnum_var45_hace3 + num_med_var45_ult3ANDnum_var45_ult1ANDnum_med_var45_ult3 + num_med_var45_ult3ANDnum_var45_ult1ANDnum_var45_hace3 + num_med_var45_ult3ANDnum_var45_ult1ANDnum_var45_ult1
                   + num_var45_hace2ANDnum_var45_hace2ANDnum_med_var45_ult3 + num_var45_hace2ANDnum_var45_hace2ANDnum_var45_hace2 + num_var45_hace2ANDnum_var45_hace2ANDnum_var45_ult1
                   + num_var45_hace2ANDnum_var45_ult1ANDnum_var45_ult1
                   + num_var45_hace3ANDnum_var45_hace3ANDnum_med_var45_ult3 + num_var45_hace3ANDnum_var45_hace3ANDnum_var45_hace3 + num_var45_hace3ANDnum_var45_hace3ANDnum_var45_ult1
                   + num_var45_hace3ANDnum_var45_ult1ANDnum_var45_ult1
                   + num_var45_ult1ANDnum_var45_ult1ANDnum_var45_ult1
                   , family = binomial(link = "logit"), data = fixed_data[1:nrow(train), ])
  temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
  fixed_data_features <- cbind(fixed_data_features, var45GLM = temporary_out)
  cat("Variable 45 AUC: ", auc(actual = train_target, predicted = temporary_out[1:nrow(train)]), "\n", sep = "")
  
  return(fixed_data_features)
}


linear_interactions_folds <- function(fixed_data, train_target, train, test_folds) {
  
  
  
  fixed_features <- cbind(fixed_data,
                          var1GLM = rep(0, nrow(fixed_data)),
                          var3GLM = rep(0, nrow(fixed_data)),
                          var4GLM = rep(0, nrow(fixed_data)),
                          var5GLM = rep(0, nrow(fixed_data)),
                          var8GLM = rep(0, nrow(fixed_data)),
                          var9GLM = rep(0, nrow(fixed_data)),
                          var10GLM = rep(0, nrow(fixed_data)),
                          var11GLM = rep(0, nrow(fixed_data)),
                          var12GLM = rep(0, nrow(fixed_data)),
                          var13GLM = rep(0, nrow(fixed_data)),
                          var14GLM = rep(0, nrow(fixed_data)),
                          var15GLM = rep(0, nrow(fixed_data)),
                          var16GLM = rep(0, nrow(fixed_data)),
                          var17GLM = rep(0, nrow(fixed_data)),
                          var20GLM = rep(0, nrow(fixed_data)),
                          var21GLM = rep(0, nrow(fixed_data)),
                          var22GLM = rep(0, nrow(fixed_data)),
                          var24GLM = rep(0, nrow(fixed_data)),
                          var25GLM = rep(0, nrow(fixed_data)),
                          var26GLM = rep(0, nrow(fixed_data)),
                          var30GLM = rep(0, nrow(fixed_data)),
                          var31GLM = rep(0, nrow(fixed_data)),
                          var32GLM = rep(0, nrow(fixed_data)),
                          var33GLM = rep(0, nrow(fixed_data)),
                          var35GLM = rep(0, nrow(fixed_data)),
                          var36GLM = rep(0, nrow(fixed_data)),
                          var37GLM = rep(0, nrow(fixed_data)),
                          var38GLM = rep(0, nrow(fixed_data)),
                          var39GLM = rep(0, nrow(fixed_data)),
                          var41GLM = rep(0, nrow(fixed_data)),
                          var42GLM = rep(0, nrow(fixed_data)),
                          var43GLM = rep(0, nrow(fixed_data)),
                          var44GLM = rep(0, nrow(fixed_data)),
                          var45GLM = rep(0, nrow(fixed_data)))
  
  
  for (i in names(test_folds)) {
    
    
    fixed_data_features <- fixed_data
    
    cat("-----\n\nRunning fold ", which(i == names(test_folds)), "/", NROW(names(test_folds)), "\n", sep = "")
    
    
    # Variable 1
    temporary <- glm(target ~ saldo_var1 + num_var1_0 + num_var1 + saldo_var1ANDsaldo_var1 + num_var1_0ANDnum_var1_0, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var1GLM = temporary_out)
    cat("Variable 1 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5050) \n", sep = "")
    
    # Variable 3
    temporary <- glm(target ~ var3 + var3ANDvar3, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var3GLM = temporary_out)
    cat("Variable 3 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5049)\n", sep = "")
    
    # Variable 4
    temporary <- glm(target ~ num_var4 + num_var4ANDnum_var4 + num_var4ANDnum_var4ANDnum_var4, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var4GLM = temporary_out)
    cat("Variable 4 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.6965)\n", sep = "")
    
    # Variable 5
    temporary <- glm(target ~ num_var5_0 + num_var5 + saldo_var5 + num_meses_var5_ult3 + saldo_medio_var5_hace2 + saldo_medio_var5_hace3 + saldo_medio_var5_ult1 + saldo_medio_var5_ult3
                     + num_var5_0ANDnum_var5_0 + num_var5_0ANDnum_var5 + num_var5_0ANDsaldo_var5 + num_var5_0ANDnum_meses_var5_ult3 + num_var5_0ANDsaldo_medio_var5_hace2 + num_var5_0ANDsaldo_medio_var5_ult1 + num_var5_0ANDsaldo_medio_var5_ult3
                     + num_var5ANDnum_var5 + num_var5ANDsaldo_var5 + num_var5ANDnum_meses_var5_ult3 + num_var5ANDsaldo_medio_var5_hace2 + num_var5ANDsaldo_medio_var5_hace3 + num_var5ANDsaldo_medio_var5_ult1 + num_var5ANDsaldo_medio_var5_ult3
                     + saldo_var5ANDsaldo_var5 + saldo_var5ANDnum_meses_var5_ult3 + saldo_var5ANDsaldo_medio_var5_hace2 + saldo_var5ANDsaldo_medio_var5_hace3 + saldo_var5ANDsaldo_medio_var5_ult1 + saldo_var5ANDsaldo_medio_var5_ult3
                     + num_meses_var5_ult3ANDnum_meses_var5_ult3 + num_meses_var5_ult3ANDsaldo_medio_var5_hace3 + num_meses_var5_ult3ANDsaldo_medio_var5_ult1 + num_meses_var5_ult3ANDsaldo_medio_var5_ult3
                     + saldo_medio_var5_hace2ANDsaldo_medio_var5_hace2 + saldo_medio_var5_hace2ANDsaldo_medio_var5_ult1 + saldo_medio_var5_hace2ANDsaldo_medio_var5_ult3
                     + saldo_medio_var5_hace3ANDsaldo_medio_var5_hace3 + saldo_medio_var5_hace3ANDsaldo_medio_var5_ult1 + saldo_medio_var5_hace3ANDsaldo_medio_var5_ult3
                     + saldo_medio_var5_ult1ANDsaldo_medio_var5_ult1 + saldo_medio_var5_ult1ANDsaldo_medio_var5_ult3 + saldo_medio_var5_ult3ANDsaldo_medio_var5_ult3,
                     family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var5GLM = temporary_out)
    cat("Variable 5 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.7118)\n", sep = "")
    
    # Variable 8
    temporary <- glm(target ~ num_var8_0 + num_var8 + saldo_var8 + saldo_medio_var8_hace2, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var8GLM = temporary_out)
    cat("Variable 8 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5240)\n", sep = "")
    
    # Variable 9
    temporary <- glm(target ~ ind_var9_cte_ult1 + ind_var9_ult1, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var9GLM = temporary_out)
    cat("Variable 9 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5029)\n", sep = "")
    
    # Variable 10
    temporary <- glm(target ~ ind_var10_ult1 + ind_var10cte_ult1, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var10GLM = temporary_out)
    cat("Variable 10 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5045)\n", sep = "")
    
    # Variable 11
    temporary <- glm(target ~ num_trasp_var11_ult1, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var11GLM = temporary_out)
    cat("Variable 11 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5035)\n", sep = "")
    
    # Variable 12
    temporary <- glm(target ~ ind_var12_0 + num_var12 + saldo_var12 + num_meses_var12_ult3, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var12GLM = temporary_out)
    cat("Variable 12 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5268)\n", sep = "")
    
    # Variable 13
    temporary <- glm(target ~ ind_var13_0 + num_aport_var13_hace3, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var13GLM = temporary_out)
    cat("Variable 13 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5230)\n", sep = "")
    
    # Variable 14
    temporary <- glm(target ~ ind_var14_0 + ind_var14 + saldo_var14, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var14GLM = temporary_out)
    cat("Variable 14 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5065)\n", sep = "")
    
    # Variable 15
    temporary <- glm(target ~ var15 + var15ANDvar15 + var15ANDvar15ANDvar15, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var15GLM = temporary_out)
    cat("Variable 15 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.7020)\n", sep = "")
    
    # Variable 16
    temporary <- glm(target ~ imp_sal_var16_ult1 + num_sal_var16_ult1 + imp_ent_var16_ult1 + num_ent_var16_ult1
                     + imp_sal_var16_ult1ANDimp_sal_var16_ult1 + imp_sal_var16_ult1ANDnum_sal_var16_ult1 + imp_sal_var16_ult1ANDimp_ent_var16_ult1 + imp_sal_var16_ult1ANDnum_ent_var16_ult1
                     + num_sal_var16_ult1ANDnum_sal_var16_ult1 + num_sal_var16_ult1ANDimp_ent_var16_ult1 + num_sal_var16_ult1ANDnum_ent_var16_ult1
                     + imp_ent_var16_ult1ANDimp_ent_var16_ult1 + imp_ent_var16_ult1ANDnum_ent_var16_ult1
                     + num_ent_var16_ult1ANDnum_ent_var16_ult1,
                     family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var16GLM = temporary_out)
    cat("Variable 16 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.4993)\n", sep = "")
    
    # Variable 17
    temporary <- glm(target ~ ind_var17 + num_meses_var17_ult3, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var17GLM = temporary_out)
    cat("Variable 17 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5009)\n", sep = "")
    
    # Variable 20
    temporary <- glm(target ~ num_var20_0, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var20GLM = temporary_out)
    cat("Variable 20 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5021)\n", sep = "")
    
    # Variable 21
    temporary <- glm(target ~ var21 + var21ANDvar21, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var21GLM = temporary_out)
    cat("Variable 21 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5010)\n", sep = "")
    
    # Variable 22
    temporary <- glm(target ~ num_var22_hace2 + num_var22_ult1 + num_var22_ult3 + num_med_var22_ult3, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var22GLM = temporary_out)
    cat("Variable 22 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5133)\n", sep = "")
    
    # Variable 24
    temporary <- glm(target ~ saldo_var24, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var24GLM = temporary_out)
    cat("Variable 24 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5157)\n", sep = "")
    
    # Variable 25
    temporary <- glm(target ~ ind_var25_cte + num_var25_0 + saldo_var25 + num_var25_0ANDsaldo_var25 , family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var25GLM = temporary_out)
    cat("Variable 25 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5132)\n", sep = "")
    
    # Variable 26
    temporary <- glm(target ~ ind_var26_cte + ind_var26_0 + num_var26_0 + saldo_var26 + num_var26_0ANDnum_var26_0 + num_var26_0ANDsaldo_var26 + saldo_var26ANDsaldo_var26, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var26GLM = temporary_out)
    cat("Variable 26 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5131)\n", sep = "")
    
    # Variable 30
    temporary <- glm(target ~ ind_var30_0 + ind_var30 + saldo_var30, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var30GLM = temporary_out)
    cat("Variable 30 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.7008)\n", sep = "")
    
    # Variable 31
    temporary <- glm(target ~ ind_var31_0 + ind_var31 + num_var31 + num_var31ANDind_var31 + num_var31ANDnum_var31, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var31GLM = temporary_out)
    cat("Variable 31 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5023)\n", sep = "")
    
    # Variable 32
    temporary <- glm(target ~ ind_var32_cte + saldo_var32 + num_var32_0ANDnum_var32_0 + num_var32_0ANDsaldo_var32 + ind_var32ANDsaldo_var32 + saldo_var32ANDsaldo_var32, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var32GLM = temporary_out)
    cat("Variable 32 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5006)\n", sep = "")
    
    # Variable 33
    temporary <- glm(target ~ ind_var33_0 +ind_var33 + num_var33_0 + num_var33 + saldo_var33 + delta_imp_aport_var33_1y3 + saldo_medio_var33_ult1, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var33GLM = temporary_out)
    cat("Variable 33 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5005)\n", sep = "")
    
    # Variable 35
    temporary <- glm(target ~ num_var35 + num_var35ANDnum_var35 + num_var35ANDnum_var35ANDnum_var35, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var35GLM = temporary_out)
    cat("Variable 35 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.6954)\n", sep = "")
    
    # Variable 36
    temporary <- glm(target ~ var36 + var36ANDvar36 + var36ANDvar36ANDvar36 + var36ANDvar36ANDvar36ANDvar36, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var36GLM = temporary_out)
    cat("Variable 36 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.6463)\n", sep = "")
    
    # Variable 37
    temporary <- glm(target ~ saldo_var37 + imp_trans_var37_ult1 + saldo_var37ANDsaldo_var37 + saldo_var37ANDimp_trans_var37_ult1 + imp_trans_var37_ult1ANDimp_trans_var37_ult1, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var37GLM = temporary_out)
    cat("Variable 37 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5194)\n", sep = "")
    
    # Variable 38
    temporary <- glm(target ~ var38, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var38GLM = temporary_out)
    cat("Variable 38 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5924)\n", sep = "")
    
    # Variable 39
    temporary <- glm(target ~
                       ind_var39_0ANDind_var39_0 + ind_var39_0ANDnum_op_var39_ult1 + ind_var39_0ANDimp_op_var39_comer_ult3 + ind_var39_0ANDimp_op_var39_efect_ult3 + ind_var39_0ANDnum_meses_var39_vig_ult3 + ind_var39_0ANDnum_op_var39_efect_ult3
                     + num_var39_0ANDind_var39 + num_var39_0ANDimp_op_var39_comer_ult1 + num_var39_0ANDimp_op_var39_efect_ult1 + num_var39_0ANDimp_op_var39_ult1 + num_var39_0ANDnum_op_var39_ult1 + num_var39_0ANDnum_op_var39_comer_ult1 + num_var39_0ANDnum_op_var39_efect_ult1 + num_var39_0ANDnum_op_var39_hace2 + num_var39_0ANDimp_op_var39_comer_ult3 + num_var39_0ANDimp_op_var39_efect_ult3 + num_var39_0ANDnum_op_var39_hace3 + num_var39_0ANDnum_meses_var39_vig_ult3 + num_var39_0ANDnum_op_var39_comer_ult3 + num_var39_0ANDnum_op_var39_efect_ult3
                     + ind_var39ANDind_var39 + ind_var39ANDimp_op_var39_comer_ult1 + ind_var39ANDimp_op_var39_efect_ult1 + ind_var39ANDimp_op_var39_ult1 + ind_var39ANDnum_op_var39_ult1 + ind_var39ANDnum_op_var39_comer_ult1 + ind_var39ANDnum_op_var39_efect_ult1 + ind_var39ANDnum_op_var39_hace2 + ind_var39ANDimp_op_var39_comer_ult3 + ind_var39ANDimp_op_var39_efect_ult3 + ind_var39ANDnum_op_var39_hace3 + ind_var39ANDnum_meses_var39_vig_ult3 + ind_var39ANDnum_op_var39_comer_ult3 + ind_var39ANDnum_op_var39_efect_ult3
                     + ind_op_var39_comer_ult1ANDimp_op_var39_comer_ult1 + ind_op_var39_comer_ult1ANDimp_op_var39_efect_ult1 + ind_op_var39_comer_ult1ANDimp_op_var39_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_comer_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_efect_ult1 + ind_op_var39_comer_ult1ANDnum_op_var39_hace2 + ind_op_var39_comer_ult1ANDimp_op_var39_comer_ult3 + imp_op_var39_comer_ult1ANDimp_op_var39_efect_ult3 + imp_op_var39_comer_ult1ANDnum_op_var39_hace3 + imp_op_var39_comer_ult1ANDnum_meses_var39_vig_ult3 + imp_op_var39_comer_ult1ANDnum_op_var39_comer_ult3 + imp_op_var39_comer_ult1ANDnum_op_var39_efect_ult3
                     + imp_op_var39_efect_ult1ANDimp_op_var39_efect_ult1 + imp_op_var39_efect_ult1ANDimp_op_var39_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_comer_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_efect_ult1 + imp_op_var39_efect_ult1ANDnum_op_var39_hace2 + imp_op_var39_efect_ult1ANDimp_op_var39_comer_ult3 + imp_op_var39_efect_ult1ANDnum_op_var39_hace3 + imp_op_var39_efect_ult1ANDnum_meses_var39_vig_ult3 + imp_op_var39_efect_ult1ANDnum_op_var39_comer_ult3 + imp_op_var39_efect_ult1ANDnum_op_var39_efect_ult3
                     + imp_op_var39_ult1ANDimp_op_var39_ult1 + imp_op_var39_ult1ANDnum_op_var39_ult1 + imp_op_var39_ult1ANDnum_op_var39_comer_ult1 + imp_op_var39_ult1ANDnum_op_var39_efect_ult1 + imp_op_var39_ult1ANDnum_op_var39_hace2 + imp_op_var39_ult1ANDimp_op_var39_comer_ult3 + imp_op_var39_ult1ANDimp_op_var39_efect_ult3 + imp_op_var39_ult1ANDnum_op_var39_hace3 + imp_op_var39_ult1ANDnum_meses_var39_vig_ult3 + imp_op_var39_ult1ANDnum_op_var39_comer_ult3 + imp_op_var39_ult1ANDnum_op_var39_efect_ult3
                     + num_op_var39_ult1ANDnum_op_var39_ult1 + num_op_var39_ult1ANDnum_op_var39_comer_ult1 + num_op_var39_ult1ANDnum_op_var39_efect_ult1 + num_op_var39_ult1ANDnum_op_var39_hace2 + num_op_var39_ult1ANDimp_op_var39_comer_ult3 + num_op_var39_ult1ANDimp_op_var39_efect_ult3 + num_op_var39_ult1ANDnum_meses_var39_vig_ult3 + num_op_var39_ult1ANDnum_op_var39_comer_ult3 + num_op_var39_ult1ANDnum_op_var39_efect_ult3
                     + num_op_var39_comer_ult1ANDnum_op_var39_comer_ult1 + num_op_var39_comer_ult1ANDnum_op_var39_efect_ult1 + num_op_var39_comer_ult1ANDnum_op_var39_hace2 + num_op_var39_comer_ult1ANDimp_op_var39_comer_ult3 + num_op_var39_comer_ult1ANDimp_op_var39_efect_ult3 + num_op_var39_comer_ult1ANDnum_op_var39_hace3 + num_op_var39_comer_ult1ANDnum_meses_var39_vig_ult3 + num_op_var39_comer_ult1ANDnum_op_var39_comer_ult3 + num_op_var39_comer_ult1ANDnum_op_var39_efect_ult3
                     + num_op_var39_efect_ult1ANDnum_op_var39_efect_ult1 + num_op_var39_efect_ult1ANDnum_op_var39_hace2 + num_op_var39_efect_ult1ANDimp_op_var39_comer_ult3 + num_op_var39_efect_ult1ANDimp_op_var39_efect_ult3 + num_op_var39_efect_ult1ANDnum_op_var39_hace3 + num_op_var39_efect_ult1ANDnum_meses_var39_vig_ult3 + num_op_var39_efect_ult1ANDnum_op_var39_comer_ult3 + num_op_var39_efect_ult1ANDnum_op_var39_efect_ult3
                     + num_op_var39_hace2ANDnum_op_var39_hace2 + num_op_var39_hace2ANDimp_op_var39_comer_ult3 + num_op_var39_hace2ANDimp_op_var39_efect_ult3 + num_op_var39_hace2ANDnum_op_var39_hace3 + num_op_var39_hace2ANDnum_meses_var39_vig_ult3 + num_op_var39_hace2ANDnum_op_var39_comer_ult3 + num_op_var39_hace2ANDnum_op_var39_efect_ult3
                     + imp_op_var39_comer_ult3ANDimp_op_var39_comer_ult3 + imp_op_var39_comer_ult3ANDimp_op_var39_efect_ult3 + imp_op_var39_comer_ult3ANDnum_op_var39_hace3 + imp_op_var39_comer_ult3ANDnum_meses_var39_vig_ult3 + imp_op_var39_comer_ult3ANDnum_op_var39_comer_ult3 + imp_op_var39_comer_ult3ANDnum_op_var39_efect_ult3
                     + imp_op_var39_efect_ult3ANDimp_op_var39_efect_ult3 + imp_op_var39_efect_ult3ANDnum_op_var39_hace3 + imp_op_var39_efect_ult3ANDnum_meses_var39_vig_ult3 + imp_op_var39_efect_ult3ANDnum_op_var39_comer_ult3 + imp_op_var39_efect_ult3ANDnum_op_var39_efect_ult3
                     + num_op_var39_hace3ANDnum_meses_var39_vig_ult3
                     + num_meses_var39_vig_ult3ANDnum_meses_var39_vig_ult3 + num_meses_var39_vig_ult3ANDnum_op_var39_comer_ult3 + num_meses_var39_vig_ult3ANDnum_op_var39_efect_ult3
                     + num_op_var39_comer_ult3ANDnum_op_var39_comer_ult3 + num_op_var39_comer_ult3ANDnum_op_var39_efect_ult3 + num_op_var39_efect_ult3ANDnum_op_var39_efect_ult3,
                     family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var39GLM = temporary_out)
    cat("Variable 39 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5819)\n", sep = "")
    
    # Variable 41
    temporary <- glm(target ~ num_var41_0 + imp_op_var41_comer_ult1 + imp_op_var41_efect_ult1 + imp_op_var41_ult1 + num_op_var41_efect_ult1
                     + num_var41_0ANDimp_op_var41_comer_ult1 + num_var41_0ANDimp_op_var41_ult1 + imp_op_var41_comer_ult1ANDnum_op_var41_efect_ult1 + imp_op_var41_efect_ult1ANDimp_op_var41_efect_ult1
                     , family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var41GLM = temporary_out)
    cat("Variable 41 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5526)\n", sep = "")
    
    # Variable 42
    temporary <- glm(target ~ num_var42_0 + num_var42 + saldo_var42
                     + num_var42_0ANDnum_var42_0 + num_var42_0ANDnum_var42 + saldo_var42ANDsaldo_var42 + saldo_var42ANDsaldo_var42ANDsaldo_var42
                     , family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var42GLM = temporary_out)
    cat("Variable 42 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.6821)\n", sep = "")
    
    # Variable 43
    temporary <- glm(target ~ ind_var43_emit_ult1 + num_var43_emit_ult1 + ind_var43_recib_ult1 + num_var43_recib_ult1, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var43GLM = temporary_out)
    cat("Variable 43 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5203)\n", sep = "")
    
    # Variable 44
    temporary <- glm(target ~ num_meses_var44_ult3, family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var44GLM = temporary_out)
    cat("Variable 44 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5008)\n", sep = "")
    
    # Variable 45
    temporary <- glm(target ~ num_med_var45_ult3 + num_var45_hace2 + num_var45_hace3 + num_var45_ult1
                     + num_med_var45_ult3ANDnum_var45_ult1
                     + num_var45_hace2ANDnum_var45_hace2 + num_var45_hace2ANDnum_var45_ult1
                     + num_var45_hace3ANDnum_var45_hace3 + num_var45_hace3ANDnum_var45_ult1
                     + num_var45_ult1ANDnum_var45_ult1
                     + num_med_var45_ult3ANDnum_med_var45_ult3 + num_med_var45_ult3ANDnum_var45_hace2 + num_med_var45_ult3ANDnum_var45_hace3 + num_med_var45_ult3ANDnum_var45_ult1ANDnum_med_var45_ult3 + num_med_var45_ult3ANDnum_var45_ult1ANDnum_var45_hace3 + num_med_var45_ult3ANDnum_var45_ult1ANDnum_var45_ult1
                     + num_var45_hace2ANDnum_var45_hace2ANDnum_med_var45_ult3 + num_var45_hace2ANDnum_var45_hace2ANDnum_var45_hace2 + num_var45_hace2ANDnum_var45_hace2ANDnum_var45_ult1
                     + num_var45_hace2ANDnum_var45_ult1ANDnum_var45_ult1
                     + num_var45_hace3ANDnum_var45_hace3ANDnum_med_var45_ult3 + num_var45_hace3ANDnum_var45_hace3ANDnum_var45_hace3 + num_var45_hace3ANDnum_var45_hace3ANDnum_var45_ult1
                     + num_var45_hace3ANDnum_var45_ult1ANDnum_var45_ult1
                     + num_var45_ult1ANDnum_var45_ult1ANDnum_var45_ult1
                     , family = binomial(link = "logit"), data = fixed_data[seq(1, nrow(train))[-test_folds[[i]]], ])
    temporary_out <- as.numeric(predict(temporary, newdata = fixed_data, type = "response"))
    fixed_data_features <- cbind(fixed_data_features, var45GLM = temporary_out)
    cat("Variable 45 AUC: ", sprintf("%.06f", auc(actual = train_target[seq(1, nrow(train))[test_folds[[i]]]], predicted = temporary_out[seq(1, nrow(train))[test_folds[[i]]]])), " (Expected: ~0.5411)\n", sep = "")
    
    tempStr <- c("var1GLM", "var3GLM", "var4GLM", "var5GLM", "var8GLM", "var9GLM",
                 "var10GLM", "var11GLM", "var12GLM", "var13GLM", "var14GLM", "var15GLM", "var16GLM", "var17GLM",
                 "var20GLM", "var21GLM", "var22GLM", "var24GLM", "var25GLM", "var26GLM",
                 "var30GLM", "var31GLM", "var32GLM", "var33GLM", "var35GLM", "var36GLM", "var37GLM", "var38GLM", "var39GLM",
                 "var41GLM", "var42GLM", "var43GLM", "var44GLM", "var45GLM")
    
    for (j in tempStr) {
      fixed_features[[j]][test_folds[[i]]] <- fixed_data_features[[j]][test_folds[[i]]]
      fixed_features[[j]][(nrow(train)+1):(nrow(fixed_data))] <- fixed_features[[j]][(nrow(train)+1):(nrow(fixed_data))] + (fixed_data_features[[j]][(nrow(train)+1):(nrow(fixed_data))] / NROW(names(test_folds)))
    }
    
    cat("Merged data set from fold.\n", sep = "")
    
    
  }
  
  
  return(fixed_features)
}
