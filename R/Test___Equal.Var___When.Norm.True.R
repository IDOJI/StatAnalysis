Test___Equal.Var___When.Norm.True = function(df, var_response, var_group, is.normal, alpha){
  data.list = as_list_by(df, var_group)

  results.list = list(NA, NA, NA, NA)
  names(results.list) = c("Equal.Var_results", "Equal.Var_What.Test","Equal.Var_p.val", "is.Equal.Var")

  if(length(data.list)==2){
    results.list[[1]] = var.test(data.list[[1]][,var_response] %>% unlist,
                                 data.list[[2]][,var_response] %>% unlist,
                                 conf.level = 1-alpha)
    results.list[[2]] = "F.test"
    results.list[[3]] = results.list[[1]]$p.value
  }else if(length(data.list)>=3){
    results.list[[1]] = bartlett.test(SUB___as.formula(var_response, var_group), df)
    results.list[[2]] = "Bartlett"
    results.list[[3]] = results.list[[1]]$p.value
  }

  results.list[[4]] = results.list[[3]] > alpha
  return(results.list)
}

