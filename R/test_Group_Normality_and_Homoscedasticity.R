test_Group_Normality_and_Homoscedasticity = function(X, group, variable, alpha=0.05){
  ### 0) Exclude NA case =============================================================
  which_na = X[,group] %>% unlist %>% is.na %>% which
  if(length(which_na)>0){
    X_new = X[-which_na, ]
    X_na = X[which_na, ]
  }else{
    X_new = X
    which_na = NULL
    X_na = NULL
  }


  ### 1) Normality =============================================================
  norm.list = test_Normality(X_new, group, variable, alpha)

  ### 2) Homoscedasticity =============================================================
  homo.df = test_Homoscedasticity(X_new, group, variable, alpha, is.normal = norm.list[[2]])

  ### 3) Combining results =============================================================
  results.df = ccbind(X = norm.list[[1]], Y = homo.df)

  return(list(results.df, is.norm=norm.list$is.normal, is.balanced=norm.list$is.balanced, is.homo=homo.df$is.EqualVar, list(which_na, X_na)))
}



# ### have na?
# group.vec = data.df[,group] %>% unlist
# if(is.na(group.vec) %>% sum > 0){
#   data.df_new = data.df[!is.na(group.vec),]
#   ind_na = which(is.na(group.vec))
#   NA_ind = paste("NA", paste("(", paste(ind_na, collapse=","), ")", sep=""), sep="")
# }else{
#   data.df_new = data.df
#   NA_ind = NULL
# }
