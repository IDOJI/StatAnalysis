test_Group_Normality_and_Homoscedasticity = function(data.df, group, variable, alpha=0.05){

  ### have na?
  group.vec = data.df[,group] %>% unlist
  if(is.na(group.vec) %>% sum > 0){
    data.df_new = data.df[-is.na(group.vec),]
    ind_na = which(is.na(group.vec))
  }

  ### group
  group.list = as_list_by(data.df_new, group, messaging = F)
  results_group.df = names(group.list) %>% data.frame #@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  names(results_group.df) = paste("Group", group, sep="_")

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
  results_norm.df = do.call(rbind, norm.list) #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  ### 2) Homoscedasticity
  homo.df = test_Homoscedasticity(data.df_new, group, variable, alpha, Norm_p.val = results_norm.df$Normality_p.val)
  empty_row.df = matrix(rep(c(" ", NA), nrow(results_group.df)-1), ncol=2, byrow=T) %>% as.data.frame
  names(empty_row.df) = names(homo.df)
  results_homo.df = rbind(homo.df, empty_row.df)

  ### return results
  results.list = list()
  results.list[[1]] = cbind(results_group.df, results_norm.df, results_homo.df)
  results.list[[2]] = data.frame(NA_index=ind_na)
  return(results.list)
}
