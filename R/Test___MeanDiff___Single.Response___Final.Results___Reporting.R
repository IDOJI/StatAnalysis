Test___MeanDiff___Single.Response___Final.Results___Reporting = function(Final.df){
  selected.df = Final.df %>% dplyr::select(c(!!"Group(Trt)", !!"Each.Group",!!"n_obs", !!"is.normal", !!"is.Equal.Var",
                                             !!"Response", !!"MeanDiff_Method", !!"MeanDiff_p.value",
                                             !!"group1", !!"group2", !!"PostHoc_p.value_adj", !!"PostHoc_p.value.signif"))

  return(selected.df)
}
