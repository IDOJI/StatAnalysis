Test___Equal.Var = function(df, var_group, var_response, is.normal, alpha=0.05){
  ### Normality : TRUE
  homo_reslts.list = Test___Equal.Var___When.Norm.True(df, var_response, var_group, is.normal, alpha)

  ### Normality : FALSE
  homo_reslts.list = Test___Equal.Var___When.Norm.False(df, var_response, var_group, is.normal, alpha)

  return(homo_reslts.list)
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
