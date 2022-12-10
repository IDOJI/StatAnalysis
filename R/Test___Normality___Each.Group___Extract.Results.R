Test___Normality___Each.Group___Extract.Results = function(Norm.test_results.list){
  ### what tests
  tests = sapply(Norm.test_results.list, FUN=function(ith_results){
    ith_results[[2]] %>% return
  });names(tests)=NULL


  ### pvalues
  pvals = sapply(Norm.test_results.list, FUN=function(ith_results){
    ith_results[[3]] %>% return
  });names(pvals)=NULL


  ### normality
  is.normal = sapply(Norm.test_results.list, FUN=function(ith_results){
    ith_results[[4]] %>% return
  })

  ### n_obs
  n_obs = sapply(Norm.test_results.list, FUN=function(ith_results){
    ith_results[[5]] %>% return
  });names(n_obs)=NULL


  ### combining
  combined.df = data.frame(Each.Group=names(is.normal), n_obs=n_obs, Norm_Tests=tests, Norm_p.val=pvals, is.normal = is.normal) %>% dplyr::as_tibble()
  rownames(combined.df) = NULL
  is.normal = sum(combined.df$is.normal)==nrow(combined.df)

  return(list(Norm_reslts=combined.df, is.normal=is.normal))
}



