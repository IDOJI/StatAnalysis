Test___Corr___Extract.Results = function(Corr.Test_results.list){
  Corr.Test_results.df = Corr.Test_results.list[[1]]


  ### rename
  Corr.Test_results.df = Corr.Test_results.df %>% dplyr::rename("x"="parameter1")
  Corr.Test_results.df = Corr.Test_results.df %>% dplyr::rename("y"="parameter2")


  ### add variable
  Corr.Test_results.df = Corr.Test_results.df %>% dplyr::mutate(alpha = 1-conf.level)
  Corr.Test_results.df = Corr.Test_results.df %>% dplyr::mutate(p.value.significancy = SUB___P.vals.Signif.Stars(p.value))

  ### select
  Corr.Test_results.df = Corr.Test_results.df %>% dplyr::select(x, y, n.obs, method, estimate, p.value, p.value.significancy, alpha, )

  return(Corr.Test_results.df)
}


#
# Results_Extracted.list = list()
# Results_Extracted.list[[1]] = Corr.Test_results
# Results_Extracted.list[[2]] = Corr.Test_results$method
# Results_Extracted.list[[3]] = Corr.Test_results$estimate
# Results_Extracted.list[[4]] = Corr.Test_results$p.value
# Results_Extracted.list[[5]] = SUB___P.vals.Signif.Stars(Corr.Test_results$p.value)
# Results_Extracted.list[[6]] = Corr.Test_results$p.value <= alpha
# names(Results_Extracted.list) = c("Test Results", "Method", "Estimate", "p.value", "p.value.significancy", "is.significant")
