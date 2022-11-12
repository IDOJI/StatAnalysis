test_MeanDiff_3group = function(data.df, group, variable, is.normal, is.homo, alpha=0.05, alpha_PostHoc=0.05, round.digits=100){
  ### have na?
  # variable = variables[1]
  group.vec = data.df[,group] %>% unlist
  if(is.na(group.vec) %>% sum > 0){
    data.df_new = data.df[!is.na(group.vec),]
    ind_na = which(is.na(group.vec))
  }else{
    data.df_new = data.df
    ind_na = NULL
  }

  cont_var = data.df_new[,variable] %>% unlist
  group_var = data.df_new[,group] %>% unlist
  n_group = data.df_new[,group] %>% unlist %>% unique %>% length

  is.balanced = table(group_var) %>% as.vector %>% unique %>% length == 1
  is.large_sample = sum((table(group_var) %>% as.vector)>=30)==n_group


  #=============================================================================
  # criterion
  #=============================================================================
  ### parametric
  ## norm O / homo O
  c1 = is.balanced && is.normal && is.homo # One-way ANOVA(Balanced)
  c2 = !is.balanced && is.normal && is.homo # One-way ANOVA(Unbalanced)
  ## homo X
  c3 = is.balanced && is.normal && !is.homo
  c4 = !is.balanced && is.normal && !is.homo
  ## norm X
  c5 = is.balanced && !is.normal
  c6 = !is.balanced && !is.normal

  # if(sum(c1,c2,c3)>1 || sum(c1,c2,c3)==0){
  #   stop("Check the criteria!")
  # }



  #=============================================================================
  # MeanDiff
  #=============================================================================
  if(c1){
    # One-way ANOVA : 3그룹이상, 대표본, 정규성O, 등분산O, balanced
    results_MeanDiff = aov(cont_var~group_var)
    results_MeanDiff_summary = summary(results_MeanDiff)
    p.val_MeanDiff = round(results_MeanDiff_summary[[1]][1,5] %>% as.numeric, round.digits)
    TestType_MeanDiff = "One-Way ANOVA(Balanced)"
    criterion = c1
  }else if(c2){
    # One-way ANOVA : 3그룹이상, 대표본, 정규성O, 등분산O, Imbalanced
    results_MeanDiff = aov(cont_var~group_var)
    results_MeanDiff_summary = summary(results_MeanDiff)
    p.val_MeanDiff = round(results_MeanDiff_summary[[1]][1,5] %>% as.numeric, round.digits)
    TestType_MeanDiff = "One-Way ANOVA(Imbalanced)"
    criterion = c2
  }else if(c3){
    # Welch ANOVA : 3그룹이상, 정규성O, 등분산X
    results_MeanDiff = oneway.test(cont_var ~ group_var, var.equal = F)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    TestType_MeanDiff = "Welch ANOVA(Balanced)"
    criterion = c3
  }else if(c4){
    # Welch ANOVA : 3그룹이상, 정규성O, 등분산X
    results_MeanDiff = oneway.test(cont_var ~ group_var, var.equal = F)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    TestType_MeanDiff = "Welch ANOVA(Imbalanced)"
    criterion = c4
  }else if(c5){
    # Kruskal-Wallis : 3그룹, 정규성 X, 소표본
    # unequal도 OK?
    results_MeanDiff = kruskal.test(cont_var~group_var)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    TestType_MeanDiff = "Kruskal-Wallis(Balanced)"
    criterion = c5
  }else if(c6){
    # Kruskal-Wallis : 3그룹, 정규성 X, 소표본
    # unequal도 OK?
    results_MeanDiff = kruskal.test(cont_var~group_var)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    TestType_MeanDiff = "Kruskal-Wallis(Imbalanced)"
    criterion = c6
  }else{
    warning("Check the criteria (3group)")
  }
  results.df = data.frame(MeanDiff_TestType = c(TestType_MeanDiff, rep(" ", n_group-1)),
                          MeanDiff_p.val = c(p.val_MeanDiff, rep(NA, n_group-1)))
  if(length(ind_na)>0){
    results.df = rbind(results.df, c(" ", NA))
  }
  results.df$MeanDiff_p.val = results.df$MeanDiff_p.val %>% as.numeric %>% suppressWarnings

  #=============================================================================
  # Post-Hoc : Multiple Comparison
  #=============================================================================
  if(results.df$MeanDiff_p.val[1]<=alpha){
    results.list = list()
    results.list[[1]] = results.df
    results.list[[2]] = test_MeanDiff_PostHoc(data.df, group, variable, alpha_PostHoc, MeanDiff_results.df = results.df)
    return(results.list)
  }else{
    return(results.df)
  }
}
