test_Group_Normality_and_Homoscedasticity = function(data.df, group, variable, alpha=0.05){
  # variable = variables[1]
  ### have na?

  group.vec = data.df[,group] %>% unlist
  if(is.na(group.vec) %>% sum > 0){
    data.df_new = data.df[!is.na(group.vec),]
    ind_na = which(is.na(group.vec))
    NA_ind = paste("NA", paste("(", paste(ind_na, collapse=","), ")", sep=""), sep="")
  }else{
    data.df_new = data.df
    NA_ind = NULL
  }

  ### group
  group.list = as_list_by(data.df_new, group, messaging = F)
  results_group.df = names(group.list) %>% data.frame
  names(results_group.df) = paste("Group", group, sep=" : ")


  ### cont variable of each group
  x.list = list()
  for(i in 1:length(group.list)){
    # i=1
    x.list[[i]] = group.list[[i]][,variable] %>% as.vector %>% unlist
  }


  ### 1) Normality
  norm.list = list()
  for(n in 1:length(x.list)){
    norm.list[[n]] = test_Normality(x.list[[n]])
  }
  results_norm.df = do.call(rbind, norm.list)

  ### 2) Homoscedasticity
  homo.df = test_Homoscedasticity(data.df_new, group, variable, alpha, Norm_p.val = results_norm.df$Normality_p.val)
  add_na = matrix(NA, nrow(results_norm.df)-nrow(homo.df), ncol(homo.df)) %>% as.data.frame
  names(add_na) = names(homo.df)
  results_homo.df = rbind(homo.df, add_na)


  ### return results
  if(!is.null(NA_ind)){
    results.df = cbind(results_group.df, results_norm.df, results_homo.df)
    na_row = c(NA_ind, rep(NA, ncol(results.df)-1))
    results.df = rbind(results.df, na_row)
  }else{
    results.df = cbind(results_group.df, results_norm.df, results_homo.df)
  }

  ### as numeric p.val
  p.val_col = which_col(results.df, which.col = "_p.val")
  for(i in 1:length(p.val_col)){
    # i=1
    results.df[,p.val_col[i]] = as.numeric(results.df[,p.val_col[i]]) %>% suppressWarnings()
  }

  return(results.df)
}

