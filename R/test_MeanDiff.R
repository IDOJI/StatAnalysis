test_MeanDiff = function(data.df, group, variable, exclude_rows=NULL, ex_reasons=NULL, alpha=0.05, round.digits=100){
  # data.df = data
  # group = "Years"
  # group = "Ethnicity"
  # variable = "WELLNESS"
  # variable = variables[m]

  ### exclude some rows
  # ex_reasons = "Small Sample Size"
  if(!is.null(exclude_rows)){
    excluded = data.df[exclude_rows,group] %>% unlist
    data.df = data.df[-exclude_rows,]
  }

  ### Normality & Homoscedasticity
  results_NormHomo.df = test_Group_Normality_and_Homoscedasticity(data.df, group, variable, alpha)

  ### group vectors
  cont_var = data.df[,variable] %>% unlist
  group_var = data.df[,group] %>% unlist %>% as.factor
  n_group = group_var %>% unique %>% length

  ### p.val
  norm_p.val = exclude_na(results_NormHomo.df$Normality_p.val)
  homo_p.val = exclude_na(results_NormHomo.df$Homoscedasticity_p.val)

  ### criterion
  is.normal = sum(norm_p.val>alpha)==nrow(results_NormHomo.df)
  is.homo = sum(homo_p.val>alpha)==2

  ### MeanDiff
  if(n_group==1){
    stop("There is only one group.")
  }else if(n_group==2){
    results_MeanDiff = test_MeanDiff_2group(data.df, group, variable, is.normal, is.homo, alpha, round.digits)
  }else{
    results_MeanDiff = test_MeanDiff_3group(data.df, group, variable, is.normal, is.homo, alpha, round.digits)
  }

  ### excluding vec & response col
  response_col_1 = c(variable, rep(" ", n_group-1))
  if(is.null(exclude_rows)){
    excluded.vec = rep(" ", length(response_col_1))
  }else{
    if(is.null(ex_reasons)){
      bec = NULL
    }else{
      bec = "∵"
    }
    excluded.vec = c(c(exclude_rows, excluded, paste("(", bec, ex_reasons, ")", sep="")),
                     rep(" ", length(response_col_1)-3))
  }

  ### post hoc?
  if(is.data.frame(results_MeanDiff)){
    final_results = cbind(Response=response_col_1, Excluded_Group_Rows=excluded.vec,results_NormHomo.df, results_MeanDiff)
  }else{
    response_col_2 = c(variable, rep(" ", nrow(results_MeanDiff[[2]])-1))
    final_results = list()
    final_results[[1]] = cbind(Response=response_col_1, Excluded_Group_Rows=excluded.vec, results_NormHomo.df, results_MeanDiff[[1]])
    final_results[[2]] = cbind(Response=response_col_2, results_MeanDiff[[2]])
  }
  return(final_results)
}

|# 1)독립성: 독립변수의 그룹 군은 서로 독립적 이여야 한다.
# 2)정규성: 독립변수에 따른 종속변수는 정규분포를 만족해야한다.
# 3)등분산성: 독립변수에 따른 종속변수 분포의 분산은 각 군마다 동일하다.
