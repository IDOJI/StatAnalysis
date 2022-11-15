test_MeanDiff_2group = function(data.df, group, variable, is.normal, is.homo, round.digits=100){
  cont_var = data.df[,variable] %>% unlist
  group_var = data.df[,group] %>% unlist %>% as.factor

  n_group = data.df[,group] %>% unlist %>% unique %>% length
  is.large_sample = sum((table(group_var) %>% as.vector)>=30)==n_group

  # have_middle_sample = sum((10<(table(group_var) %>% as.vector))==((table(group_var) %>% as.vector)<30))>1
  # have_small_sample = sum((table(group_var) %>% as.vector)<10)>1
  # is.balanced = table(group_var) %>% as.vector %>% unique %>% length == 1


  #=============================================================================
  # criterion
  #=============================================================================
  # norm O
  c1 = is.normal && is.homo # t
  c2 = is.normal && !is.homo # welch
  # norm X / large O
  c3 = !is.normal && is.homo && is.large_sample # t
  c4 = !is.normal && !is.homo && is.large_sample # welch
  # norm X / large X
  c5 = !is.normal && !is.large_sample # nonparametric

  if(sum(c1,c2,c3,c4,c5)!=1){
    stop("Check the criteria!")
  }

  # test
  if(c1){
    # t.test : 2그룹, 정규성O, 등분산O
    results_MeanDiff = t.test(cont_var~group_var, var.equal=T)
    p.val_MeanDiff = round(results_MeanDiff$p.value  %>% as.numeric, round.digits)
    TestType_MeanDiff = "t.test"
  }else if(c2){
    # t.test : 2그룹, 정규성O, 등분산X
    results_MeanDiff = t.test(cont_var~group_var, var.equal=F)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    # %>% format(scientific=T)
    TestType_MeanDiff = "Welch"
  }else if(c3){
    # t.test : 2그룹, 정규성X, 등분산O, 대표본
    results_MeanDiff = t.test(cont_var~group_var, var.equal=T)
    p.val_MeanDiff = round(results_MeanDiff$p.value  %>% as.numeric, round.digits)
    TestType_MeanDiff = "t.test(Asymptotic)"
  }else if(c4){
    # t.test : 2그룹, 정규성X, 등분산X, 대표본
    results_MeanDiff = t.test(cont_var~group_var, var.equal=F)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    # %>% format(scientific=T)
    TestType_MeanDiff = "Welch(Asymptotic)"
  }else if(c5){
    # Mann-Whitney : 정규성X -> 비모수 검정
    results_MeanDiff = wilcox.test(cont_var~group_var)
    p.val_MeanDiff = round(results_MeanDiff$p.value %>% as.numeric, round.digits)
    TestType_MeanDiff = "Mann-Whitney"
  }
  results.df = data.frame(MeanDiff_TestType = c(TestType_MeanDiff, rep(" ", n_group-1)),
                          MeanDiff_p.val = c(p.val_MeanDiff, rep(NA, n_group-1)))
  return(results.df)
}
