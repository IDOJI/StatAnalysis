Test___Normality___Each.Group = function(df, var_group, var_response, alpha=0.05){
  data.list = as_list_by(df, var_group)
  results.list = lapply(data.list, FUN=function(ith_df, ...){
    # ith_df = data.list[[6]]
    norm.test = Test___Normality(ith_df[,var_response] %>% unlist, alpha)
    norm.test[[5]] = nrow(ith_df)
    names(norm.test)[5] = "n_obs"
    return(norm.test)
  })
  return(results.list)
}



