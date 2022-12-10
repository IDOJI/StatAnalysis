test___MeanDiff___Single.Response___Final.Results = function(Norm.test_combined.df, Equal.Var.test_combined.df, Mean.Diff.Results){
  Norm.EqVar.df = ccbind(Norm.test_combined.df, Equal.Var.test_combined.df)


  Mean.Diff.Results[[1]]$expression = NULL
  Mean.Diff.Results[[2]]$expression = NULL
  Mean.Diff.df = ccbind(Mean.Diff.Results[[1]], Mean.Diff.Results[[2]])

  Final.df = ccbind(Norm.EqVar.df, Mean.Diff.df) %>% dplyr::as_tibble()


  ##############################################################################
  Reporting.df = test___MeanDiff___Single.Response___Final.Results___Reporting(Final.df)
  return(list(Final.df, Reporting.df))
}
