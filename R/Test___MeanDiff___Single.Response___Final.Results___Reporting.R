Test___MeanDiff___Single.Response___Final.Results___Reporting = function(Final.df){
  if(!"group1" %in% names(Final.df)){
    selected.df = Final.df %>% dplyr::select(c(!!"Group(Trt)", !!"Each.Group",!!"n_obs",
                                               # !!"is.normal", !!"is.Equal.Var",
                                               !!"Response",
                                               # !!"MeanDiff_Method",
                                               !!"MeanDiff_p.value", !!"MeanDiff_p.value.signif"))

  }else{
    selected.df = Final.df %>% dplyr::select(c(!!"Group(Trt)", !!"Each.Group",!!"n_obs",
                                               # !!"is.normal", !!"is.Equal.Var",
                                               !!"Response",
                                               # !!"MeanDiff_Method",
                                               !!"MeanDiff_p.value", !!"MeanDiff_p.value.signif",
                                               !!"group1", !!"group2", !!"PostHoc_p.value_adj", !!"PostHoc_p.value.signif"))

  }
  return(selected.df)
}
