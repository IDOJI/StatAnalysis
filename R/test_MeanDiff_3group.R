test_MeanDiff_3group = function(data.df, group, variable, is.normal, is.homo, alpha=0.05, round.digits=100){
  n_group = data.df[,group] %>% unlist %>% unique %>% length
  is.balanced = table(group_var) %>% as.vector %>% unique %>% length == 1
  is.large_sample = sum((table(group_var) %>% as.vector)>=30)==n_group
  # have_middle_sample = sum((10<(table(group_var) %>% as.vector))==((table(group_var) %>% as.vector)<30))>1
  # have_small_sample = sum((table(group_var) %>% as.vector)<10)>1

  #=============================================================================
  # criterion
  #=============================================================================
  ### parametric
  ## norm O / homo O
  c1 = is.balanced && is.normal && is.homo # One-way ANOVA(Balanced)
  c2 = !is.balanced && is.normal && is.homo # One-way ANOVA(Unbalanced)
  ## norm X
  c3 = !is.normal #

  # if(sum(c1,c2,c3)>1 || sum(c1,c2,c3)==0){
  #   stop("Check the criteria!")
  # }

  # test
  if(c1){
    # One-way ANOVA : 3그룹이상, 대표본, 정규성O, 등분산O, balanced
    results_MeanDiff = aov(cont_var~group_var) %>% summary
    p.val_MeanDiff = round(results_MeanDiff[[1]][1,5] %>% as.numeric, round.digits)
    TestType_MeanDiff = "One-Way ANOVA(Balanced)"
  }else if(c2){
    # One-way ANOVA : 3그룹이상, 대표본, 정규성O, 등분산O, Imbalanced
    results_MeanDiff = aov(cont_var~group_var) %>% summary
    p.val_MeanDiff = round(results_MeanDiff[[1]][1,5] %>% as.numeric, round.digits)
    TestType_MeanDiff = "One-Way ANOVA(Imbalanced)"
  }else if(c3){
    # Kruskal-Wallis : 3그룹, 정규성 X, 소표본
    # unequal도 OK?
    results_MeanDiff = kruskal.test(cont_var~group_var)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    TestType_MeanDiff = "Kruskal-Wallis"
  }else{
    warning("Check the criteria (3group)")
  }
  results.df = data.frame(MeanDiff_TestType = c(TestType_MeanDiff, rep(" ", n_group-1)),
                          MeanDiff_p.val = c(p.val_MeanDiff, rep(NA, n_group-1)))

  return(results.df)
}
