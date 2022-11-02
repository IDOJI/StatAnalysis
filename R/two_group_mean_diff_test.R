two_group_mean_diff_test = function(data.df, group, variable, alpha=0.05, alternative="two.sided", round.digits=100){
  # 1)독립성: 독립변수의 그룹 군은 서로 독립적 이여야 한다.
  # 2)정규성: 독립변수에 따른 종속변수는 정규분포를 만족해야한다.
  # 3)등분산성: 독립변수에 따른 종속변수 분포의 분산은 각 군마다 동일하다.
  # data.df = data
  # group = "Gender"
  # variable = "WELLNESS"


  ### 0) group
  grouped.list = as_list_by(data.df, group, messaging = F)
  group_1 = grouped.list[[1]]
  group_2 = grouped.list[[2]]

  name_group_1 = unique(group_1[,group]) %>% unlist
  name_group_2 = unique(group_2[,group]) %>% unlist

  x1 = group_1[,variable] %>% as.vector %>% unlist
  x2 = group_2[,variable] %>% as.vector %>% unlist

  results.list = list()

  #=============================================================================
  # 1) normality : 정규성검정
  #=============================================================================
  x1_norm = shapiro.test(x1)
  x2_norm = shapiro.test(x2)
  x1_pval = x1_norm$p.value %>% format(,scientific=F) %>% as.numeric
  x2_pval = x2_norm$p.value %>% format(,scientific=F) %>% as.numeric
  normality = list(round(x1_pval, round.digits), round(x2_pval, round.digits))
  names(normality) = c(name_group_1, name_group_2)
  results.list[[1]] = normality
  names(results.list)[1] = "Normality(Shapiro.test)_p.val"

  #=============================================================================
  # 2) homoscedasticity : 등분산검정
  #=============================================================================
  results_var.test = var.test(x1, x2)
  var_pval = format(results_var.test$p.value, scientific = F) %>% as.numeric
  results.list[[2]] = round(var_pval,round.digits)
  names(results.list)[2] = "Homoscedasticity_p.val"

  # normal O
  is_normal = x1_norm$p.value>alpha && x2_norm$p.value>alpha
  if(is_normal){
    if(results_var.test$p.value > alpha){
      #=============================================================================
      # 3) t.test : 정규성O, 등분산O -> pooled variance를 이용한 t-test를 적용한다 (var.equal=TRUE)
      #=============================================================================
      results_t.test = t.test(x1, x2, var.equal = T)
      t.test_pval = results_t.test$p.value %>% format(scientific=F) %>% as.numeric
      results.list[[3]] = round(t.test_pval, round.digits)
      names(results.list)[3] = "Mean_difference(T.test)"
    }else{
      #=============================================================================
      # 3) t.test : 정규성O, 등분산X -> Welch의 t-test를 적용한다 (t.test())
      #=============================================================================
      results_t.test = t.test(x1, x2, var.equal = F)
      t.test_pval = results_t.test$p.value %>% format(scientific=F) %>% as.numeric
      results.list[[3]] = round(t.test_pval, round.digits)
      names(results.list)[3] = "Mean_difference(Welch)"
    }
  }else{
    #=============================================================================
    # 3) Mann-Whitney : 정규성X -> 비모수 검정
    #=============================================================================
    results_mean.test = wilcox.test(x1, x2, alternative, conf.level = 1-alpha)
    mean.test_pval = results_mean.test$p.value %>% format(scientific=F) %>% as.numeric
    results.list[[3]] = round(mean.test_pval, round.digits)
    names(results.list)[3] = "Mean_difference(Mann-Whitney)"
  }
  return(results.list)
}
