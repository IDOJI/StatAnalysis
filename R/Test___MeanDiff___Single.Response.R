Test___MeanDiff___Single.Reponse = function(##############################
                                          # data & variables
                                          ##############################
                                          df, var_group, var_response,
                                          ##############################
                                          # Significance level
                                          ##############################
                                          alpha_Norm  = 0.05,
                                          alpha_Equal.Var  = 0.05,
                                          alpha_ANOVA = 0.05,
                                          alpha_PostHoc = 0.05,
                                          p.adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                                          ##############################
                                          # Figure
                                          ##############################
                                          # title="",
                                          # results.subtitle=T,
                                          ##############################
                                          # exporting
                                          ##############################
                                          save.path,
                                          filename,
                                          export.xlsx = F){
  #============================================================================
  # Exclude NA
  #============================================================================
  which_NA = df[,var_group] %>% unlist %>% is.na %>% which
  if(length(which_NA)>0){
    df = df[-which_NA, ]
  }



  #============================================================================
  # Normality
  #============================================================================
  Norm.Test_results.list  = Test___Normality___Each.Group(df, var_group, var_response, alpha_Norm)
  Norm.Test_combined.list = Test___Normality___Each.Group___Extract.Results(Norm.Test_results.list)
  Norm.Test_combined.df   = Norm.Test_combined.list[[1]]
  is.Normal               = Norm.Test_combined.list[[2]]




  #============================================================================
  # Equal.Var
  #============================================================================
  Equal.Var.Test_results.list = Test___Equal.Var(df, var_group, var_response, is.Normal, alpha_Equal.Var)
  Equal.Var.Test_combined.df  = Test___Equal.Var___Extract.Results(Equal.Var.Test_results.list)
  is.Equal.Var = Equal.Var.Test_combined.df$is.Equal.Var





  #============================================================================
  # Meandiff results
  #============================================================================
  p = Test___MeanDiff___Single.Response___ggstats(# data & variables
                                                  df,
                                                  var_group,
                                                  var_response,
                                                  # test results
                                                  alpha_ANOVA,
                                                  is.Normal,
                                                  is.Equal.Var,
                                                  # plotting options
                                                  results.subtitle = T,
                                                  pairwise.comparisons = T,
                                                  p.adjust.method,
                                                  title="")
  Mean.Diff.Results = Test___MeanDiff___Single.Response___Results.Extractor(p, var_group, var_response)



  #============================================================================
  # Boxplot & save
  #============================================================================
  if(is.null(Mean.Diff.Results$Post.Hoc)){
    if(Mean.Diff.Results[[1]]$MeanDiff_p.value <= alpha_ANOVA){
      p = Test___MeanDiff___Single.Response___Box.Plot(df, var_group, var_response, Mean.Diff.Results, alpha_ANOVA=alpha_ANOVA, palette = "jco")
      ggsave(filename = paste0(filename, ".png"), plot = p, path = save.path)
      cat("\n", crayon::red(paste0(filename, ".png")),  crayon::blue("is saved !"),"\n")
    }
  }else{
    if(sum(unlist(Mean.Diff.Results[[2]]$PostHoc_p.value_adj) <= alpha_PostHoc) != 0){
      p = Test___MeanDiff___Single.Response___Box.Plot(df, var_group, var_response, Mean.Diff.Results, alpha_PostHoc=alpha_PostHoc, palette = "jco")
      ggsave(filename = paste0(filename, ".png"), plot = p, path = save.path)
      cat("\n", crayon::red(paste0(filename, ".png")),  crayon::blue("is saved !"),"\n")
    }
  }



  #============================================================================
  # final results
  #============================================================================
  Final.list = Test___MeanDiff___Single.Response___Final.Results(save.path,
                                                                 filename,
                                                                 Norm.Test_combined.df,
                                                                 Equal.Var.Test_combined.df,
                                                                 Mean.Diff.Results)

  #============================================================================
  # export final results
  #============================================================================
  if(export.xlsx){
    Test___MeanDiff___Exporting.XLSX.Highlight(Final.df,
                                               Reporting.df,
                                               alpha_ANOVA,
                                               alpha_PostHoc,
                                               save.path,
                                               filename)
  }


  return(Final.list)
}
