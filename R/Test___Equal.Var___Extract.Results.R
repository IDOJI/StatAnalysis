Test___Equal.Var___Extract.Results = function(Equal.Var.Test_results.list){
  results = Equal.Var.Test_results.list[2:4] %>% unlist
  results.df = results %>% matrix(1,3) %>% as.data.frame
  names(results.df) = names(results)
  results.df$Equal.Var_p.val = as.numeric(results.df$Equal.Var_p.val)
  results.df$is.Equal.Var = as.logical(results.df$is.Equal.Var)
  results.df = dplyr::as_tibble(results.df)
  return(results.df)
}
