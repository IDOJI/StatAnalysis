Test___Equal.Var___When.Norm.False = function(df, var_response, var_group, is.normal, alpha){
  #===========================================================================
  # as list
  #===========================================================================
  data.list = as_list_by(df, var_group)



  #===========================================================================
  # Check outliers
  #===========================================================================
  have_outliers = sapply(data.list, FUN=function(ith_df, ...){
    #ith_df = data.list[[1]]
    SUB___Have.Outlier(ith_df[,var_response] %>% unlist, by="IQR") %>% return
  })
  have_outliers = sum(have_outliers)==length(data.list)


  #===========================================================================
  # test
  #===========================================================================
  results.list = list(NA, NA, NA, NA)
  names(results.list) = c("Equal.Var_results", "Equal.Var_What.Test", "Equal.Var_p.val", "is.Equal.Var")

  # when problems related to outliers in the dataset cannot be resolved.
  if(have_outliers){
    results.list[[1]] = fligner.test(SUB___as_formula(y=var_response, x=var_group), data=df)
    results.list[[2]] = "Fligner-Killeen"
    results.list[[3]] = results.list[[1]]$p.value %>% as.numeric
  }else{
    results.list[[1]] =car::leveneTest(y=df[,var_response] %>% unlist,  group=df[,var_group] %>% unlist) %>% suppressWarnings()
    results.list[[2]] = "Levene"
    results.list[[3]] = results.list[[1]]$`Pr(>F)`[1] %>% as.numeric
  }

  results.list[[4]] = results.list[[3]] > alpha
  return(results.list)
}
