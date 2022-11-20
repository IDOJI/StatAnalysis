test_Homoscedasticity = function(X, group, variable, alpha=0.05, is.normal=T){
  ### Grouping data =============================================================
  Group.list = as_list_by(X, group)
  if(length(Group.list)==2){
    NA.list = Group.list[[2]]
    Group.list = Group.list[[1]]
  }
  ### Extract a variable for each group =========================================
  X.list = lapply(Group.list, variable, FUN=function(g, ...){
    return(g[,variable] %>% unlist)
  })



  ### 정규성 O =================================================================
  if(is.normal){
    ### F-test : 정규성 O, 2그룹
    if(length(X.list)==2){
      results = var.test(X.list[[1]], X.list[[2]], conf.level = 1-alpha)
      results.df = data.frame("F.test", results$p.value)
    ### Bartlett : 정규성 O, 3그룹이상
    }else{
      results = bartlett.test(formula=as_formula(y=variable, x=group), data=X)
      results.df = data.frame("Bartlett", results$p.value)
    }
  ### 정규성 X =================================================================
  }else{
    sample_size = sapply(X.list, length)>30
    ### Levene : 정규성 X, 대표본 (30이상)
    if(sum(sample_size)==length(X.list)){
      results = car::leveneTest(y=X[,variable] %>% unlist, group=X[,group] %>% unlist)
      results.df = data.frame("Levene", results$`Pr(>F)`[1])
    ### Fligner-Killeen : 정규성X, 소표본 (30미만) -> 비모수적방법
    }else{
      results = fligner.test(formula=as_formula(y=variable, x=group), data=X)
      results.df = data.frame("Fligner-Killeen", results$p.value)
    }
  }





  ### Results ==================================================================
  is.homo = results.df[,2]>alpha
  results.df = cbind(results.df, is.homo)
  names(results.df) = c(paste("Equal.Var", c("TestType", "p.val"), sep="_"), "is.EqualVar")
  return(results.df)
}



# Paper : An Adjustment to the Bartlett's Test for Small Sample Size
# In conclusion,
# the adjustment has good control on the type I error and higher power,
# and thus is recommended for small samples and large population number
# when underlying distribution is normal.


# 3) 레빈
# 등분산 검정중 하나인 Levene 검정은
# 집단간 분산이 같은지 다른지 여부를 알아볼 때 사용하기도 하고
# 독립 2표본 t-검정 또는 일원분산분석(one-way ANOVA) 실시 전에
# 가정 때문에 확인하는 용도로 사용하기도 한다.
# 그리고 Levene 검정은 두 집단 뿐만 아니라
# 세 집단 이상에서도 사용할 수 있으며
# Bartlett 검정과 달리 표본이 정규성을 보이지 않아도 사용할 수 있다.


# 4) 비모수
# "Fligner-Killeen Test of Homogeneity of Variances,
# Performs a Fligner-Killeen (median) test of the null
# that the variances in each of the groups (samples) are the same"
