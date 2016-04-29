CreateFeatures <- function(data, which, diff = TRUE, ratio = TRUE, indicator = TRUE, numeric = TRUE) {
  
  # data = the data set
  # which = the variable base to use
  # diff = compute differences if available?
  # ratio = compute ratios if available?
  # indicator = summarize indicators if available?
  # numeric = summarize numerics if available?
  
  IndicatorMaker <- function(data, nums) {
    temp_num <- data[[nums[1]]]
    for (i in nums[-1]) {
      temp_num <- temp_num + data[[nums[i]]]
    }
    return(temp_num)
  }
  
  SumMaker <- function(data, nums) {
    return(IndicatorMaker(data, nums))
  }
  
  DiffMaker <- function(data, num1, num2) {
    temp_num <- data[[num1]] - data[[num2]]
    return(temp_num)
  }
  
  RatioMaker <- function(data, num1, num2) {
    temp_num <- data[[num1]]
    temp_num[!(data[[num2]] == 0)] <- data[[num1]][!(data[[num2]] == 0)] / data[[num2]][!(data[[num2]] == 0)] #divide only by non infinite
    return(temp_num)
  }
  
  temp_list <- data.frame(ZZZ = rep(0, nrow(data)))
  
  if (which == "var1") {
    
    # VARIABLE 1
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var1SUM = IndicatorMaker(data, c("ind_var1_0", "ind_var1")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var1SUM = SumMaker(data, c("num_var1_0", "num_var1")))}

  } else if (which == "var2") {
    
    # VARIABLE 2
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var2SUM = IndicatorMaker(data, c("ind_var2_0", "ind_var2")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var2SUM = SumMaker(data, c("num_var2_0_ult1", "num_var2_ult1")))}

  } else if (which == "var5") {
    
    # VARIABLE 5
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var5SUM = IndicatorMaker(data, c("ind_var5_0", "ind_var5")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          saldo_medio_var5_haceDIFF = DiffMaker(data, "saldo_medio_var5_hace2", "saldo_medio_var5_hace3"),
                                          saldo_medio_var5_ultDIFF = DiffMaker(data, "saldo_medio_var5_ult1", "saldo_medio_var5_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           saldo_medio_var5_haceRATIO = RatioMaker(data, "saldo_medio_var5_hace2", "saldo_medio_var5_hace3"),
                                           saldo_medio_var5_ultRATIO = RatioMaker(data, "saldo_medio_var5_ult1", "saldo_medio_var5_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var5SUM = SumMaker(data, c("num_var5_0", "num_var5")),
                                             num_var5SUMAll = SumMaker(data, c("num_var5_0", "num_var5", "num_meses_var5_ult3")),
                                             saldo_medio_var5_haceSUM = SumMaker(data, c("saldo_medio_var5_hace2", "saldo_medio_var5_hace3")),
                                             saldo_medio_var5_ultSUM = SumMaker(data, c("saldo_medio_var5_ult1", "saldo_medio_var5_ult3")))}
    
  } else if (which == "var6") {
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var6SUM = IndicatorMaker(data, c("ind_var6_0", "ind_var6")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var6SUM = SumMaker(data, c("num_var6_0", "num_var6")))}
    
  } else if (which == "var7") {
    
    # VARIABLE 7
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var7SUM = IndicatorMaker(data, c("ind_var7_emit_ult1", "ind_var7_recib_ult1")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          imp_var7DIFF = DiffMaker(data, "imp_var7_emit_ult1", "imp_var7_recib_ult1"),
                                          num_var7DIFF = DiffMaker(data, "num_var7_emit_ult1", "num_var7_recib_ult1"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           imp_var7RATIO = RatioMaker(data, "imp_var7_emit_ult1", "imp_var7_recib_ult1"),
                                           num_var7RATIO = RatioMaker(data, "num_var7_emit_ult1", "num_var7_recib_ult1"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var7SUM = SumMaker(data, c("num_var7_emit_ult1", "num_var7_recib_ult1")),
                                             imp_var7SUM = SumMaker(data, c("imp_var7_emit_ult1", "imp_var7_recib_ult1")),
                                             num_var7SUM = SumMaker(data, c("num_var7_emit_ult1", "num_var7_recib_ult1")))}
    
  } else if (which == "var8") {
    
    # VARIABLE 8
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var8SUM = IndicatorMaker(data, c("ind_var8_0", "ind_var8")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          saldo_medio_var8_haceDIFF = DiffMaker(data, "saldo_medio_var8_hace2", "saldo_medio_var8_hace3"),
                                          saldo_medio_var8_ultDIFF = DiffMaker(data, "saldo_medio_var8_ult1", "saldo_medio_var8_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           saldo_medio_var8_haceRATIO = RatioMaker(data, "saldo_medio_var8_hace2", "saldo_medio_var8_hace3"),
                                           saldo_medio_var8_ultRATIO = RatioMaker(data, "saldo_medio_var8_ult1", "saldo_medio_var8_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var8SUM = SumMaker(data, c("num_var8_0", "num_var8")),
                                             num_var8SUMAll = SumMaker(data, c("num_var8_0", "num_var8", "num_meses_var8_ult3")),
                                             saldo_medio_var8_haceSUM = SumMaker(data, c("saldo_medio_var8_hace2", "saldo_medio_var8_hace3")),
                                             saldo_medio_var8_ultSUM = SumMaker(data, c("saldo_medio_var8_ult1", "saldo_medio_var8_ult3")))}
    
  } else if (which == "var9") {
    
    # VARIABLE 9
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var9SUM = IndicatorMaker(data, c("ind_var9_cte_ult1", "ind_var9_ult1")))}
    
  } else if (which == "var10") {
    
    # VARIABLE 10
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var10SUM = IndicatorMaker(data, c("ind_var10_ult1", "ind_var10cte_ult1")))}
    
  } else if (which == "var12") {
    
    # VARIABLE 12
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var12SUM = IndicatorMaker(data, c("ind_var12_0", "ind_var12")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          saldo_medio_var12_haceDIFF = DiffMaker(data, "saldo_medio_var12_hace2", "saldo_medio_var12_hace3"),
                                          saldo_medio_var12_ultDIFF = DiffMaker(data, "saldo_medio_var12_ult1", "saldo_medio_var12_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           saldo_medio_var12_haceRATIO = RatioMaker(data, "saldo_medio_var12_hace2", "saldo_medio_var12_hace3"),
                                           saldo_medio_var12_ultRATIO = RatioMaker(data, "saldo_medio_var12_ult1", "saldo_medio_var12_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var12SUM = SumMaker(data, c("num_var12_0", "num_var12")),
                                             num_var12SUMAll = SumMaker(data, c("num_var12_0", "num_var12", "num_meses_var12_ult3")),
                                             saldo_medio_var12_haceSUM = SumMaker(data, c("saldo_medio_var12_hace2", "saldo_medio_var12_hace3")),
                                             saldo_medio_var12_ultSUM = SumMaker(data, c("saldo_medio_var12_ult1", "saldo_medio_var12_ult3")))}
    
  } else if (which == "var13") {
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list,
                                               ind_var13SUM = IndicatorMaker(data, c("ind_var13_0", "ind_var13")),
                                               ind_var13_cortoSUM = IndicatorMaker(data, c("ind_var13_corto_0", "ind_var13_corto")),
                                               ind_var13_largoSUM = IndicatorMaker(data, c("ind_var13_largo_0", "ind_var13_largo")),
                                               ind_var13_medioSUM = IndicatorMaker(data, c("ind_var13_medio_0", "ind_var13_medio")),
                                               ind_var13SUMAll = IndicatorMaker(data, c("ind_var13_0", "ind_var13", "ind_var13_corto_0", "ind_var13_corto", "ind_var13_largo_0", "ind_var13_largo", "ind_var13_medio_0", "ind_var13_medio")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_aport_var13DIFF = DiffMaker(data, "num_aport_var13_hace3", "num_aport_var13_ult1"),
                                          num_meses_var13DIFF = DiffMaker(data, "num_meses_var13_corto_ult3", "num_meses_var13_largo_ult3"),
                                          num_reemb_var13DIFF = DiffMaker(data, "num_reemb_var13_hace3", "num_reemb_var13_ult1"),
                                          delta_num_var13DIFF = DiffMaker(data, "delta_num_aport_var13_1y3", "delta_num_reemb_var13_1y3"),
                                          saldo_medio_var13_corto_haceDIFF = DiffMaker(data, "saldo_medio_var13_corto_hace2", "saldo_medio_var13_corto_hace3"),
                                          saldo_medio_var13_corto_ultDIFF = DiffMaker(data, "saldo_medio_var13_corto_ult1", "saldo_medio_var13_corto_ult3"),
                                          saldo_medio_var13_largo_haceDIFF = DiffMaker(data, "saldo_medio_var13_largo_hace2", "saldo_medio_var13_largo_hace3"),
                                          saldo_medio_var13_largo_ultDIFF = DiffMaker(data, "saldo_medio_var13_largo_ult1", "saldo_medio_var13_largo_ult3"),
                                          saldo_medio_var13_medio_haceDIFF = DiffMaker(data, "saldo_medio_var13_medio_hace2", "saldo_medio_var13_medio_hace3"),
                                          saldo_medio_var13_medio_ultDIFF = DiffMaker(data, "saldo_medio_var13_medio_ult1", "saldo_medio_var13_medio_ult3"),
                                          imp_aport_var13DIFF = DiffMaker(data, "imp_aport_var13_hace3", "imp_aport_var13_ult1"),
                                          imp_reemb_var13DIFF = DiffMaker(data, "imp_reemb_var13_hace3", "imp_reemb_var13_ult1"),
                                          delta_imp_var13DIFF = DiffMaker(data, "delta_imp_aport_var13_1y3", "delta_imp_reemb_var13_1y3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_aport_var13RATIO = RatioMaker(data, "num_aport_var13_hace3", "num_aport_var13_ult1"),
                                           num_meses_var13RATIO = RatioMaker(data, "num_meses_var13_corto_ult3", "num_meses_var13_largo_ult3"),
                                           num_reemb_var13RATIO = RatioMaker(data, "num_reemb_var13_hace3", "num_reemb_var13_ult1"),
                                           delta_num_var13RATIO = RatioMaker(data, "delta_num_aport_var13_1y3", "delta_num_reemb_var13_1y3"),
                                           saldo_medio_var13_corto_haceRATIO = RatioMaker(data, "saldo_medio_var13_corto_hace2", "saldo_medio_var13_corto_hace3"),
                                           saldo_medio_var13_corto_ultRATIO = RatioMaker(data, "saldo_medio_var13_corto_ult1", "saldo_medio_var13_corto_ult3"),
                                           saldo_medio_var13_largo_haceRATIO = RatioMaker(data, "saldo_medio_var13_largo_hace2", "saldo_medio_var13_largo_hace3"),
                                           saldo_medio_var13_largo_ultRATIO = RatioMaker(data, "saldo_medio_var13_largo_ult1", "saldo_medio_var13_largo_ult3"),
                                           saldo_medio_var13_medio_haceRATIO = RatioMaker(data, "saldo_medio_var13_medio_hace2", "saldo_medio_var13_medio_hace3"),
                                           saldo_medio_var13_medio_ultRATIO = RatioMaker(data, "saldo_medio_var13_medio_ult1", "saldo_medio_var13_medio_ult3"),
                                           imp_aport_var13RATIO = RatioMaker(data, "imp_aport_var13_hace3", "imp_aport_var13_ult1"),
                                           imp_reemb_var13RATIO = RatioMaker(data, "imp_reemb_var13_hace3", "imp_reemb_var13_ult1"),
                                           delta_imp_var13RATIO = RatioMaker(data, "delta_imp_aport_var13_1y3", "delta_imp_reemb_var13_1y3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var13SUM = SumMaker(data, c("num_var13_0", "num_var13")),
                                             num_var13_cortoSUM = SumMaker(data, c("num_var13_corto_0", "num_var13_corto")),
                                             num_var13_largoSUM = SumMaker(data, c("num_var13_largo_0", "num_var13_largo")),
                                             num_var13_medioSUM = SumMaker(data, c("num_var13_medio_0", "num_var13_medio")),
                                             num_var13SUMAll = SumMaker(data, c("num_var13_0", "num_var13", "num_var13_corto_0", "num_var13_corto", "num_var13_largo_0", "num_var13_largo", "num_var13_medio_0", "num_var13_medio")),
                                             num_aport_var13SUM = SumMaker(data, c("num_aport_var13_hace3", "num_aport_var13_ult1")),
                                             num_meses_var13SUM = SumMaker(data, c("num_meses_var13_corto_ult3", "num_meses_var13_largo_ult3")),
                                             num_reemb_var13SUM = SumMaker(data, c("num_reemb_var13_hace3", "num_reemb_var13_ult1")),
                                             delta_num_var13SUM = SumMaker(data, c("delta_num_aport_var13_1y3", "delta_num_reemb_var13_1y3")),
                                             saldo_medio_var13_corto_haceSUM = SumMaker(data, c("saldo_medio_var13_corto_hace2", "saldo_medio_var13_corto_hace3")),
                                             saldo_medio_var13_corto_ultSUM = SumMaker(data, c("saldo_medio_var13_corto_ult1", "saldo_medio_var13_corto_ult3")),
                                             saldo_medio_var13_largo_haceSUM = SumMaker(data, c("saldo_medio_var13_largo_hace2", "saldo_medio_var13_largo_hace3")),
                                             saldo_medio_var13_largo_ultSUM = SumMaker(data, c("saldo_medio_var13_largo_ult1", "saldo_medio_var13_largo_ult3")),
                                             saldo_medio_var13_medio_haceSUM = SumMaker(data, c("saldo_medio_var13_medio_hace2", "saldo_medio_var13_medio_hace3")),
                                             saldo_medio_var13_medio_ultSUM = SumMaker(data, c("saldo_medio_var13_medio_ult1", "saldo_medio_var13_medio_ult3")),
                                             imp_aport_var13SUM = SumMaker(data, c("imp_aport_var13_hace3", "imp_aport_var13_ult1")),
                                             imp_reemb_var13SUM = SumMaker(data, c("imp_reemb_var13_hace3", "imp_reemb_var13_ult1")),
                                             delta_imp_var13SUM = SumMaker(data, c("delta_imp_aport_var13_1y3", "delta_imp_reemb_var13_1y3")))}
    
  } else if (which == "var14") {
    
    # VARIABLE 14
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var14SUM = IndicatorMaker(data, c("ind_var14_0", "ind_var14")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var14SUM = SumMaker(data, c("num_var14_0", "num_var14")))}
    
  } else if (which == "var16") {
    
    # VARIABLE 16
    
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_var16DIFF = DiffMaker(data, "num_ent_var16_ult1", "num_sal_var16_ult1"),
                                          imp_var16DIFF = DiffMaker(data, "imp_ent_var16_ult1", "imp_sal_var16_ult1"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_var16RATIO = RatioMaker(data, "num_ent_var16_ult1", "num_sal_var16_ult1"),
                                           imp_var16RATIO = RatioMaker(data, "imp_ent_var16_ult1", "imp_sal_var16_ult1"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var16RATIO = SumMaker(data, c("num_ent_var16_ult1", "num_sal_var16_ult1")),
                                             imp_var16RATIO = SumMaker(data, c("imp_ent_var16_ult1", "imp_sal_var16_ult1")))}
    
  } else if (which == "var17") {
    
    # VARIABLE 17
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list,
                                               ind_var17SUM = IndicatorMaker(data, c("ind_var17_0", "ind_var17")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_aport_var17DIFF = DiffMaker(data, "num_aport_var17_hace3", "num_aport_var17_ult1"),
                                          num_reemb_var17DIFF = DiffMaker(data, "num_reemb_var17_hace3", "num_reemb_var17_ult1"),
                                          num_trasp_in_var17DIFF = DiffMaker(data, "num_trasp_var17_in_hace3", "num_trasp_var17_in_ult1"),
                                          num_trasp_out_var17DIFF = DiffMaker(data, "num_trasp_var17_out_hace3", "num_trasp_var17_out_ult1"),
                                          delta_num_ApRe_var17DIFF = DiffMaker(data, "delta_num_aport_var17_1y3", "delta_num_reemb_var17_1y3"),
                                          delta_num_trasp_var17DIFF = DiffMaker(data, "delta_num_trasp_var17_in_1y3", "delta_num_trasp_var17_out_1y3"),
                                          saldo_medio_var17_haceDIFF = DiffMaker(data, "saldo_medio_var17_hace2", "saldo_medio_var17_hace3"),
                                          saldo_medio_var17_ultDIFF = DiffMaker(data, "saldo_medio_var17_ult1", "saldo_medio_var17_ult3"),
                                          imp_aport_var17DIFF = DiffMaker(data, "imp_aport_var17_hace3", "imp_aport_var17_ult1"),
                                          imp_reemb_var17DIFF = DiffMaker(data, "imp_reemb_var17_hace3", "imp_reemb_var17_ult1"),
                                          imp_trasp_in_var17DIFF = DiffMaker(data, "imp_trasp_var17_in_hace3", "imp_trasp_var17_in_ult1"),
                                          imp_trasp_out_var17DIFF = DiffMaker(data, "imp_trasp_var17_out_hace3", "imp_trasp_var17_out_ult1"),
                                          delta_imp_ApRe_var17DIFF = DiffMaker(data, "delta_imp_aport_var17_1y3", "delta_imp_reemb_var17_1y3"),
                                          delta_imp_trasp_var17DIFF = DiffMaker(data, "delta_imp_trasp_var17_in_1y3", "delta_imp_trasp_var17_out_1y3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_aport_var17RATIO = RatioMaker(data, "num_aport_var17_hace3", "num_aport_var17_ult1"),
                                           num_reemb_var17RATIO = RatioMaker(data, "num_reemb_var17_hace3", "num_reemb_var17_ult1"),
                                           num_trasp_in_var17RATIO = RatioMaker(data, "num_trasp_var17_in_hace3", "num_trasp_var17_in_ult1"),
                                           num_trasp_out_var17RATIO = RatioMaker(data, "num_trasp_var17_out_hace3", "num_trasp_var17_out_ult1"),
                                           delta_num_ApRe_var17RATIO = RatioMaker(data, "delta_num_aport_var17_1y3", "delta_num_reemb_var17_1y3"),
                                           delta_num_trasp_var17RATIO = RatioMaker(data, "delta_num_trasp_var17_in_1y3", "delta_num_trasp_var17_out_1y3"),
                                           saldo_medio_var17_haceRATIO = RatioMaker(data, "saldo_medio_var17_hace2", "saldo_medio_var17_hace3"),
                                           saldo_medio_var17_ultRATIO = RatioMaker(data, "saldo_medio_var17_ult1", "saldo_medio_var17_ult3"),
                                           imp_aport_var17RATIO = RatioMaker(data, "imp_aport_var17_hace3", "imp_aport_var17_ult1"),
                                           imp_reemb_var17RATIO = RatioMaker(data, "imp_reemb_var17_hace3", "imp_reemb_var17_ult1"),
                                           imp_trasp_in_var17RATIO = RatioMaker(data, "imp_trasp_var17_in_hace3", "imp_trasp_var17_in_ult1"),
                                           imp_trasp_out_var17RATIO = RatioMaker(data, "imp_trasp_var17_out_hace3", "imp_trasp_var17_out_ult1"),
                                           delta_imp_ApRe_var17RATIO = RatioMaker(data, "delta_imp_aport_var17_1y3", "delta_imp_reemb_var17_1y3"),
                                           delta_imp_trasp_var17RATIO = RatioMaker(data, "delta_imp_trasp_var17_in_1y3", "delta_imp_trasp_var17_out_1y3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var17SUM = SumMaker(data, c("num_var17_0", "num_var17")),
                                             num_aport_var17SUM = SumMaker(data, c("num_aport_var17_hace3", "num_aport_var17_ult1")),
                                             num_reemb_var17SUM = SumMaker(data, c("num_reemb_var17_hace3", "num_reemb_var17_ult1")),
                                             num_trasp_in_var17SUM = SumMaker(data, c("num_trasp_var17_in_hace3", "num_trasp_var17_in_ult1")),
                                             num_trasp_out_var17SUM = SumMaker(data, c("num_trasp_var17_out_hace3", "num_trasp_var17_out_ult1")),
                                             delta_num_ApRe_var17SUM = SumMaker(data, c("delta_num_aport_var17_1y3", "delta_num_reemb_var17_1y3")),
                                             delta_num_trasp_var17SUM = SumMaker(data, c("delta_num_trasp_var17_in_1y3", "delta_num_trasp_var17_out_1y3")),
                                             saldo_medio_var17_haceSUM = SumMaker(data, c("saldo_medio_var17_hace2", "saldo_medio_var17_hace3")),
                                             saldo_medio_var17_ultSUM = SumMaker(data, c("saldo_medio_var17_ult1", "saldo_medio_var17_ult3")),
                                             imp_aport_var17SUM = SumMaker(data, c("imp_aport_var17_hace3", "imp_aport_var17_ult1")),
                                             imp_reemb_var17SUM = SumMaker(data, c("imp_reemb_var17_hace3", "imp_reemb_var17_ult1")),
                                             imp_trasp_in_var17SUM = SumMaker(data, c("imp_trasp_var17_in_hace3", "imp_trasp_var17_in_ult1")),
                                             imp_trasp_out_var17SUM = SumMaker(data, c("imp_trasp_var17_out_hace3", "imp_trasp_var17_out_ult1")),
                                             delta_imp_ApRe_var17SUM = SumMaker(data, c("delta_imp_aport_var17_1y3", "delta_imp_reemb_var17_1y3")),
                                             delta_imp_trasp_var17SUM = SumMaker(data, c("delta_imp_trasp_var17_in_1y3", "delta_imp_trasp_var17_out_1y3")))}
    
  } else if (which == "var18") {
    
    # VARIABLE 18
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var18SUM = IndicatorMaker(data, c("ind_var18_0", "ind_var18")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list, imp_amort_var18DIFF = DiffMaker(data, "imp_amort_var18_hace3", "imp_amort_var18_ult1"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list, imp_amort_var18RATIO = RatioMaker(data, "imp_amort_var18_hace3", "imp_amort_var18_ult1"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var18SUM = SumMaker(data, c("num_var18_0", "num_var18")),
                                             imp_amort_var18SUM = SumMaker(data, c("imp_amort_var18_hace3", "imp_amort_var18_ult1")))}
    
  } else if (which == "var20") {
    
    # VARIABLE 20
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var20SUM = IndicatorMaker(data, c("ind_var20_0", "ind_var20")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var20SUM = SumMaker(data, c("num_var20_0", "num_var20")))}
    
  } else if (which == "var22") {
    
    # VARIABLE 22
    
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_var22_haceDIFF = DiffMaker(data, "num_var22_hace2", "num_var22_hace3"),
                                          num_var22_ultDIFF = DiffMaker(data, "num_var22_ult1", "num_var22_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_var22_haceRATIO = RatioMaker(data, "num_var22_hace2", "num_var22_hace3"),
                                           num_var22_ultRATIO = RatioMaker(data, "num_var22_ult1", "num_var22_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var22_haceSUM = SumMaker(data, c("num_var22_hace2", "num_var22_hace3")),
                                             num_var22_ultSUM = SumMaker(data, c("num_var22_ult1", "num_var22_ult3")))}
    
  } else if (which == "var24") {
    
    # VARIABLE 24
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var24SUM = IndicatorMaker(data, c("ind_var24_0", "ind_var24")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var24SUM = SumMaker(data, c("num_var24_0", "num_var24")))}
    
  } else if (which == "var25") {
    
    # VARIABLE 25
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var25SUM = IndicatorMaker(data, c("ind_var25_0", "ind_var25", "ind_var25_cte")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var25SUM = SumMaker(data, c("num_var25_0", "num_var25")))}
    
  } else if (which == "var26") {
    
    # VARIABLE 26
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var26SUM = IndicatorMaker(data, c("ind_var26_0", "ind_var26", "ind_var26_cte")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var26SUM = SumMaker(data, c("num_var26_0", "num_var26")))}
    
  } else if (which == "var27") {
    
    # VARIABLE 27
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var27SUM = IndicatorMaker(data, c("ind_var27_0", "ind_var27")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var27SUM = SumMaker(data, c("num_var27_0", "num_var27_0")))}
    
  } else if (which == "var28") {
    
    # VARIABLE 28
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var28SUM = IndicatorMaker(data, c("ind_var28_0", "ind_var28")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var28SUM = SumMaker(data, c("num_var28_0", "num_var28")))}
    
  } else if (which == "var29") {
    
    # VARIABLE 29
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var28SUM = IndicatorMaker(data, c("ind_var29_0", "ind_var29")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          saldo_medio_var29_haceDIFF = DiffMaker(data, "saldo_medio_var29_hace2", "saldo_medio_var29_hace3"),
                                          saldo_medio_var29_ultDIFF = DiffMaker(data, "saldo_medio_var29_ult1", "saldo_medio_var29_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           saldo_medio_var29_haceRATIO = RatioMaker(data, "saldo_medio_var29_hace2", "saldo_medio_var29_hace3"),
                                           saldo_medio_var29_ultRATIO = RatioMaker(data, "saldo_medio_var29_ult1", "saldo_medio_var29_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var29SUM = SumMaker(data, c("num_var29_0", "num_var29_0")),
                                             num_var29SUMAll = SumMaker(data, c("num_var29_0", "num_var29_0", "num_meses_var29_ult3")),
                                             saldo_medio_var29_haceSUM = SumMaker(data, c("saldo_medio_var29_hace2", "saldo_medio_var29_hace3")),
                                             saldo_medio_var29_ultSUM = SumMaker(data, c("saldo_medio_var29_ult1", "saldo_medio_var29_ult3")))}
    
  } else if (which == "var30") {
    
    # VARIABLE 30
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var30SUM = IndicatorMaker(data, c("ind_var30_0", "ind_var30")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var30SUM = SumMaker(data, c("num_var30_0", "num_var30")))}
    
  } else if (which == "var31") {
    
    # VARIABLE 31
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var31SUM = IndicatorMaker(data, c("ind_var31_0", "ind_var31")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var31SUM = SumMaker(data, c("num_var31_0", "num_var31")))}
    
  } else if (which == "var32") {
    
    # VARIABLE 32
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var32SUM = IndicatorMaker(data, c("ind_var32_0", "ind_var32", "ind_var32_cte")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var32SUM = SumMaker(data, c("num_var32_0", "num_var32")))}
    
  } else if (which == "var33") {
    
    # VARIABLE 33
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var33SUM = IndicatorMaker(data, c("ind_var33_0", "ind_var33")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_aport_var33DIFF = DiffMaker(data, "num_aport_var33_hace3", "num_aport_var33_ult1"),
                                          num_reemb_var33DIFF = DiffMaker(data, "num_reemb_var33_hace3", "num_reemb_var33_ult1"),
                                          num_trasp_var33_inDIFF = DiffMaker(data, "num_trasp_var33_in_hace3", "num_trasp_var33_in_ult1"),
                                          num_trasp_var33_outDIFF = DiffMaker(data, "num_trasp_var33_out_hace3", "num_trasp_var33_out_ult1"),
                                          delta_num_ApRe_var33_1y3DIFF = DiffMaker(data, "delta_num_aport_var33_1y3", "delta_num_reemb_var33_1y3"),
                                          delta_num_trasp_var33DIFF = DiffMaker(data, "delta_num_trasp_var33_in_1y3", "delta_num_trasp_var33_out_1y3"),
                                          saldo_medio_var33_haceDIFF = DiffMaker(data, "saldo_medio_var33_hace2", "saldo_medio_var33_hace3"),
                                          saldo_medio_var33_ultDIFF = DiffMaker(data, "saldo_medio_var33_ult1", "saldo_medio_var33_ult3"),
                                          imp_aport_var33DIFF = DiffMaker(data, "imp_aport_var33_hace3", "imp_aport_var33_ult1"),
                                          imp_reemb_var33DIFF = DiffMaker(data, "imp_reemb_var33_hace3", "imp_reemb_var33_ult1"),
                                          imp_trasp_var33_inDIFF = DiffMaker(data, "imp_trasp_var33_in_hace3", "imp_trasp_var33_in_ult1"),
                                          imp_trasp_var33_outDIFF = DiffMaker(data, "imp_trasp_var33_out_hace3", "imp_trasp_var33_out_ult1"),
                                          delta_imp_ApRe_var33DIFF = DiffMaker(data, "delta_imp_aport_var33_1y3", "delta_imp_reemb_var33_1y3"),
                                          delta_imp_trasp_var33DIFF = DiffMaker(data, "delta_imp_trasp_var33_in_1y3", "delta_imp_trasp_var33_out_1y3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_aport_var33RATIO = RatioMaker(data, "num_aport_var33_hace3", "num_aport_var33_ult1"),
                                           num_reemb_var33RATIO = RatioMaker(data, "num_reemb_var33_hace3", "num_reemb_var33_ult1"),
                                           num_trasp_var33_inRATIO = RatioMaker(data, "num_trasp_var33_in_hace3", "num_trasp_var33_in_ult1"),
                                           num_trasp_var33_outRATIO = RatioMaker(data, "num_trasp_var33_out_hace3", "num_trasp_var33_out_ult1"),
                                           delta_num_ApRe_var33_1y3RATIO = RatioMaker(data, "delta_num_aport_var33_1y3", "delta_num_reemb_var33_1y3"),
                                           delta_num_trasp_var33RATIO = RatioMaker(data, "delta_num_trasp_var33_in_1y3", "delta_num_trasp_var33_out_1y3"),
                                           saldo_medio_var33_haceRATIO = RatioMaker(data, "saldo_medio_var33_hace2", "saldo_medio_var33_hace3"),
                                           saldo_medio_var33_ultRATIO = RatioMaker(data, "saldo_medio_var33_ult1", "saldo_medio_var33_ult3"),
                                           imp_aport_var33RATIO = RatioMaker(data, "imp_aport_var33_hace3", "imp_aport_var33_ult1"),
                                           imp_reemb_var33RATIO = RatioMaker(data, "imp_reemb_var33_hace3", "imp_reemb_var33_ult1"),
                                           imp_trasp_var33_inRATIO = RatioMaker(data, "imp_trasp_var33_in_hace3", "imp_trasp_var33_in_ult1"),
                                           imp_trasp_var33_outRATIO = RatioMaker(data, "imp_trasp_var33_out_hace3", "imp_trasp_var33_out_ult1"),
                                           delta_imp_ApRe_var33RATIO = RatioMaker(data, "delta_imp_aport_var33_1y3", "delta_imp_reemb_var33_1y3"),
                                           delta_imp_trasp_var33RATIO = RatioMaker(data, "delta_imp_trasp_var33_in_1y3", "delta_imp_trasp_var33_out_1y3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var33SUM = SumMaker(data, c("num_var31_0", "num_var31")),
                                             num_aport_var33SUM = SumMaker(data, c("num_aport_var33_hace3", "num_aport_var33_ult1")),
                                             num_reemb_var33SUM = SumMaker(data, c("num_reemb_var33_hace3", "num_reemb_var33_ult1")),
                                             num_trasp_var33_inSUM = SumMaker(data, c("num_trasp_var33_in_hace3", "num_trasp_var33_in_ult1")),
                                             num_trasp_var33_outSUM = SumMaker(data, c("num_trasp_var33_out_hace3", "num_trasp_var33_out_ult1")),
                                             delta_num_ApRe_var33_1y3SUM = SumMaker(data, c("delta_num_aport_var33_1y3", "delta_num_reemb_var33_1y3")),
                                             delta_num_trasp_var33SUM = SumMaker(data, c("delta_num_trasp_var33_in_1y3", "delta_num_trasp_var33_out_1y3")),
                                             saldo_medio_var33_haceSUM = SumMaker(data, c("saldo_medio_var33_hace2", "saldo_medio_var33_hace3")),
                                             saldo_medio_var33_ultSUM = SumMaker(data, c("saldo_medio_var33_ult1", "saldo_medio_var33_ult3")),
                                             imp_aport_var33SUM = SumMaker(data, c("imp_aport_var33_hace3", "imp_aport_var33_ult1")),
                                             imp_reemb_var33SUM = SumMaker(data, c("imp_reemb_var33_hace3", "imp_reemb_var33_ult1")),
                                             imp_trasp_var33_inSUM = SumMaker(data, c("imp_trasp_var33_in_hace3", "imp_trasp_var33_in_ult1")),
                                             imp_trasp_var33_outSUM = SumMaker(data, c("imp_trasp_var33_out_hace3", "imp_trasp_var33_out_ult1")),
                                             delta_imp_ApRe_var33SUM = SumMaker(data, c("delta_imp_aport_var33_1y3", "delta_imp_reemb_var33_1y3")),
                                             delta_imp_trasp_var33SUM = SumMaker(data, c("delta_imp_trasp_var33_in_1y3", "delta_imp_trasp_var33_out_1y3")))}
    
  } else if (which == "var34") {
    
    # VARIABLE 34
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var34SUM = IndicatorMaker(data, c("ind_var34_0", "ind_var34")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list, imp_amort_var34DIFF = DiffMaker(data, "imp_amort_var34_hace3", "imp_amort_var34_ult1"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list, imp_amort_var34RATIO = RatioMaker(data, "imp_amort_var34_hace3", "imp_amort_var34_ult1"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, imp_amort_var34SUM = SumMaker(data, c("imp_amort_var34_hace3", "imp_amort_var34_ult1")))}
    
  } else if (which == "var37") {
    
    # VARIABLE 37
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var37SUM = IndicatorMaker(data, c("ind_var37_cte", "ind_var37_0", "ind_var37")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var37SUM = SumMaker(data, c("num_var37_0", "num_var37")),
                                             num_var37SUMAll = SumMaker(data, c("num_var37_0", "num_var37", "num_var37_med_ult2")))}
    
  } else if (which == "var39") {
    
    # VARIABLE 39
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var39SUM = IndicatorMaker(data, c("ind_var39_0", "ind_var39")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_op_var39_haceDIFF = DiffMaker(data, "num_op_var39_hace2", "num_op_var39_hace3"),
                                          num_op_var39_ultDIFF = DiffMaker(data, "num_op_var39_ult1", "num_op_var39_ult3"),
                                          num_op_var39_comerDIFF = DiffMaker(data, "num_op_var39_comer_ult1", "num_op_var39_comer_ult3"),
                                          num_op_var39_efectDIFF = DiffMaker(data, "num_op_var39_efect_ult1", "num_op_var39_efect_ult3"),
                                          imp_op_var39_comerDIFF = DiffMaker(data, "imp_op_var39_comer_ult1", "imp_op_var39_comer_ult3"),
                                          imp_op_var39_efectDIFF = DiffMaker(data, "imp_op_var39_efect_ult1", "imp_op_var39_efect_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                          num_op_var39_haceRATIO = RatioMaker(data, "num_op_var39_hace2", "num_op_var39_hace3"),
                                          num_op_var39_ultRATIO = RatioMaker(data, "num_op_var39_ult1", "num_op_var39_ult3"),
                                          num_op_var39_comerRATIO = RatioMaker(data, "num_op_var39_comer_ult1", "num_op_var39_comer_ult3"),
                                          num_op_var39_efectRATIO = RatioMaker(data, "num_op_var39_efect_ult1", "num_op_var39_efect_ult3"),
                                          imp_op_var39_comerRATIO = RatioMaker(data, "imp_op_var39_comer_ult1", "imp_op_var39_comer_ult3"),
                                          imp_op_var39_efectRATIO = RatioMaker(data, "imp_op_var39_efect_ult1", "imp_op_var39_efect_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var39SUM = SumMaker(data, c("num_var39_0", "num_var39")),
                                             num_op_var39_haceSUM = SumMaker(data, c("num_op_var39_hace2", "num_op_var39_hace3")),
                                             num_op_var39_ultSUM = SumMaker(data, c("num_op_var39_ult1", "num_op_var39_ult3")),
                                             num_op_var39_comerSUM = SumMaker(data, c("num_op_var39_comer_ult1", "num_op_var39_comer_ult3")),
                                             num_op_var39_efectSUM = SumMaker(data, c("num_op_var39_efect_ult1", "num_op_var39_efect_ult3")),
                                             imp_op_var39_comerSUM = SumMaker(data, c("imp_op_var39_comer_ult1", "imp_op_var39_comer_ult3")),
                                             imp_op_var39_efectSUM = SumMaker(data, c("imp_op_var39_efect_ult1", "imp_op_var39_efect_ult3")))}
    
  } else if (which == "var40") {
    
    # VARIABLE 40
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var40SUM = IndicatorMaker(data, c("ind_var40_0", "ind_var40")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_op_var40_haceDIFF = DiffMaker(data, "num_op_var40_hace2", "num_op_var40_hace3"),
                                          num_op_var40_ultDIFF = DiffMaker(data, "num_op_var40_ult1", "num_op_var40_ult3"),
                                          num_op_var40_comerDIFF = DiffMaker(data, "num_op_var40_comer_ult1", "num_op_var40_comer_ult3"),
                                          num_op_var40_efectDIFF = DiffMaker(data, "num_op_var40_efect_ult1", "num_op_var40_efect_ult3"),
                                          imp_op_var40_comerDIFF = DiffMaker(data, "imp_op_var40_comer_ult1", "imp_op_var40_comer_ult3"),
                                          imp_op_var40_efectDIFF = DiffMaker(data, "imp_op_var40_efect_ult1", "imp_op_var40_efect_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_op_var40_haceRATIO = RatioMaker(data, "num_op_var40_hace2", "num_op_var40_hace3"),
                                           num_op_var40_ultRATIO = RatioMaker(data, "num_op_var40_ult1", "num_op_var40_ult3"),
                                           num_op_var40_comerRATIO = RatioMaker(data, "num_op_var40_comer_ult1", "num_op_var40_comer_ult3"),
                                           num_op_var40_efectRATIO = RatioMaker(data, "num_op_var40_efect_ult1", "num_op_var40_efect_ult3"),
                                           imp_op_var40_comerRATIO = RatioMaker(data, "imp_op_var40_comer_ult1", "imp_op_var40_comer_ult3"),
                                           imp_op_var40_efectRATIO = RatioMaker(data, "imp_op_var40_efect_ult1", "imp_op_var40_efect_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var40SUM = SumMaker(data, c("num_var40_0", "num_var40")),
                                             num_op_var40_haceSUM = SumMaker(data, c("num_op_var40_hace2", "num_op_var40_hace3")),
                                             num_op_var40_ultSUM = SumMaker(data, c("num_op_var40_ult1", "num_op_var40_ult3")),
                                             num_op_var40_comerSUM = SumMaker(data, c("num_op_var40_comer_ult1", "num_op_var40_comer_ult3")),
                                             num_op_var40_efectSUM = SumMaker(data, c("num_op_var40_efect_ult1", "num_op_var40_efect_ult3")),
                                             imp_op_var40_comerSUM = SumMaker(data, c("imp_op_var40_comer_ult1", "imp_op_var40_comer_ult3")),
                                             imp_op_var40_efectSUM = SumMaker(data, c("imp_op_var40_efect_ult1", "imp_op_var40_efect_ult3")))}
    
  } else if (which == "var41") {
    
    # VARIABLE 41
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var41SUM = IndicatorMaker(data, c("ind_var41_0", "ind_var41")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_op_var41_haceDIFF = DiffMaker(data, "num_op_var41_hace2", "num_op_var41_hace3"),
                                          num_op_var41_ultDIFF = DiffMaker(data, "num_op_var41_ult1", "num_op_var41_ult3"),
                                          num_op_var41_comerDIFF = DiffMaker(data, "num_op_var41_comer_ult1", "num_op_var41_comer_ult3"),
                                          num_op_var41_efectDIFF = DiffMaker(data, "num_op_var41_efect_ult1", "num_op_var41_efect_ult3"),
                                          imp_op_var41_comerDIFF = DiffMaker(data, "imp_op_var41_comer_ult1", "imp_op_var41_comer_ult3"),
                                          imp_op_var41_efectDIFF = DiffMaker(data, "imp_op_var41_efect_ult1", "imp_op_var41_efect_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_op_var41_haceRATIO = RatioMaker(data, "num_op_var41_hace2", "num_op_var41_hace3"),
                                           num_op_var41_ultRATIO = RatioMaker(data, "num_op_var41_ult1", "num_op_var41_ult3"),
                                           num_op_var41_comerRATIO = RatioMaker(data, "num_op_var41_comer_ult1", "num_op_var41_comer_ult3"),
                                           num_op_var41_efectRATIO = RatioMaker(data, "num_op_var41_efect_ult1", "num_op_var41_efect_ult3"),
                                           imp_op_var41_comerRATIO = RatioMaker(data, "imp_op_var41_comer_ult1", "imp_op_var41_comer_ult3"),
                                           imp_op_var41_efectRATIO = RatioMaker(data, "imp_op_var41_efect_ult1", "imp_op_var41_efect_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var41SUM = SumMaker(data, c("num_var41_0", "num_var41")),
                                             num_op_var41_haceSUM = SumMaker(data, c("num_op_var41_hace2", "num_op_var41_hace3")),
                                             num_op_var41_ultSUM = SumMaker(data, c("num_op_var41_ult1", "num_op_var41_ult3")),
                                             num_op_var41_comerSUM = SumMaker(data, c("num_op_var41_comer_ult1", "num_op_var41_comer_ult3")),
                                             num_op_var41_efectSUM = SumMaker(data, c("num_op_var41_efect_ult1", "num_op_var41_efect_ult3")),
                                             imp_op_var41_comerSUM = SumMaker(data, c("imp_op_var41_comer_ult1", "imp_op_var41_comer_ult3")),
                                             imp_op_var41_efectSUM = SumMaker(data, c("imp_op_var41_efect_ult1", "imp_op_var41_efect_ult3")))}
    
  } else if (which == "var43") {
    
    # VARIABLE 43
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var43SUM = IndicatorMaker(data, c("ind_var43_emit_ult1", "ind_var43_recib_ult1")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list, num_var43DIFF = DiffMaker(data, "num_var43_emit_ult1", "num_var43_recib_ult1"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list, num_var43RATIO = RatioMaker(data, "num_var43_emit_ult1", "num_var43_recib_ult1"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var43SUM = SumMaker(data, c("num_var43_emit_ult1", "num_var43_recib_ult1")))}
    
  } else if (which == "var44") {
    
    # VARIABLE 44
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var44SUM = IndicatorMaker(data, c("ind_var44_0", "ind_var44")))}
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_compra_var44DIFF = DiffMaker(data, "num_compra_var44_hace3", "num_compra_var44_ult1"),
                                          num_venta_var44DIFF = DiffMaker(data, "num_venta_var44_hace3", "num_venta_var44_ult1"),
                                          delta_num_CoVe_var44DIFF = DiffMaker(data, "delta_num_compra_var44_1y3", "delta_num_venta_var44_1y3"),
                                          saldo_medio_var44_haceDIFF = DiffMaker(data, "saldo_medio_var44_hace2", "saldo_medio_var44_hace3"),
                                          saldo_medio_var44_ultDIFF = DiffMaker(data, "saldo_medio_var44_ult1", "saldo_medio_var44_ult3"),
                                          imp_compra_var44DIFF = DiffMaker(data, "imp_compra_var44_hace3", "imp_compra_var44_ult1"),
                                          imp_venta_var44DIFF = DiffMaker(data, "imp_venta_var44_hace3", "imp_venta_var44_ult1"),
                                          delta_imp_CoVe_var44DIFF = DiffMaker(data, "delta_imp_compra_var44_1y3", "delta_imp_venta_var44_1y3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                           num_compra_var44RATIO = RatioMaker(data, "num_compra_var44_hace3", "num_compra_var44_ult1"),
                                           num_venta_var44RATIO = RatioMaker(data, "num_venta_var44_hace3", "num_venta_var44_ult1"),
                                           delta_num_CoVe_var44RATIO = RatioMaker(data, "delta_num_compra_var44_1y3", "delta_num_venta_var44_1y3"),
                                           saldo_medio_var44_haceRATIO = RatioMaker(data, "saldo_medio_var44_hace2", "saldo_medio_var44_hace3"),
                                           saldo_medio_var44_ultRATIO = RatioMaker(data, "saldo_medio_var44_ult1", "saldo_medio_var44_ult3"),
                                           imp_compra_var44RATIO = RatioMaker(data, "imp_compra_var44_hace3", "imp_compra_var44_ult1"),
                                           imp_venta_var44RATIO = RatioMaker(data, "imp_venta_var44_hace3", "imp_venta_var44_ult1"),
                                           delta_imp_CoVe_var44RATIO = RatioMaker(data, "delta_imp_compra_var44_1y3", "delta_imp_venta_var44_1y3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                             num_var44SUM = SumMaker(data, c("num_var44_0", "num_var44")),
                                             num_compra_var44SUM = SumMaker(data, c("num_compra_var44_hace3", "num_compra_var44_ult1")),
                                             num_venta_var44SUM = SumMaker(data, c("num_venta_var44_hace3", "num_venta_var44_ult1")),
                                             delta_num_CoVe_var44SUM = SumMaker(data, c("delta_num_compra_var44_1y3", "delta_num_venta_var44_1y3")),
                                             saldo_medio_var44_haceSUM = SumMaker(data, c("saldo_medio_var44_hace2", "saldo_medio_var44_hace3")),
                                             saldo_medio_var44_ultSUM = SumMaker(data, c("saldo_medio_var44_ult1", "saldo_medio_var44_ult3")),
                                             imp_compra_var44SUM = SumMaker(data, c("imp_compra_var44_hace3", "imp_compra_var44_ult1")),
                                             imp_venta_var44SUM = SumMaker(data, c("imp_venta_var44_hace3", "imp_venta_var44_ult1")),
                                             delta_imp_CoVe_var44SUM = SumMaker(data, c("delta_imp_compra_var44_1y3", "delta_imp_venta_var44_1y3")))}
    
  } else if (which == "var45") {
    
    # VARIABLE 45
    
    if (diff == TRUE) {temp_list <- cbind(temp_list,
                                          num_var45_haceDIFF = DiffMaker(data, "num_var45_hace2", "num_var45_hace3"),
                                          num_var45_ultDIFF = DiffMaker(data, "num_var45_ult1", "num_var45_ult3"))}
    if (ratio == TRUE) {temp_list <- cbind(temp_list,
                                          num_var45_haceRATIO = RatioMaker(data, "num_var45_hace2", "num_var45_hace3"),
                                          num_var45_ultRATIO = RatioMaker(data, "num_var45_ult1", "num_var45_ult3"))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list,
                                           num_var45_haceSUM = SumMaker(data, c("num_var45_hace2", "num_var45_hace3")),
                                           num_var45_ultSUM = SumMaker(data, c("num_var45_ult1", "num_var45_ult3")))}
    
  } else if (which == "var46") {
    
    # VARIABLE 46
    
    if (indicator == TRUE) {temp_list <- cbind(temp_list, ind_var46SUM = IndicatorMaker(data, c("ind_var46_0", "ind_var46")))}
    if (numeric == TRUE) {temp_list <- cbind(temp_list, num_var46SUM = SumMaker(data, c("num_var46_0", "num_var46")))}
    
  }
  
  temp_list$ZZZ <- NULL
  return(temp_list)
  
}
