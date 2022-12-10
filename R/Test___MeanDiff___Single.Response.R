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
                                          filename){
  #============================================================================
  # Normality
  #============================================================================
  Norm.Test_results.list  = Test___Normality___Each.Group(df, var_group, var_response, alpha_Norm)
  Norm.Test_combined.list = Test___Normality___Each.Group___Extract.Results(Norm.Test___results.list)
  Norm.Test_combined.df   = Norm.Test_combined.list[[1]]
  is.normal               = Norm.Test_combined.list[[2]]




  #============================================================================
  # Equal.Var
  #============================================================================
  Equal.Var.Test_results.list = Test___Equal.Var(df, var_group, var_response, is.normal, alpha_Equal.Var)
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
                                                is.normal,
                                                is.Equal.Var,
                                                # plotting options
                                                results.subtitle = T,
                                                pairwise.comparisons = T,
                                                p.adjust.method,
                                                title)
  Mean.Diff.Results = Test___MeanDiff___Single.Response___Results.Extractor(p, var_group, var_response)



  #============================================================================
  # Boxplot & save
  #============================================================================
  p = Test___MeanDiff___Single.Response___Box.Plot(df, var_group, var_response, Mean.Diff.Results, alpha_PostHoc)
  ggsave(filename = paste0(filename, ".png"), plot = p, path = save.path, dpi = 300, limitsize = T)
  cat("\n", crayon::red(filename),  crayon::blue("is saved !"),"\n")



  #============================================================================
  # final results
  #============================================================================
  # 하이라이트 기능 넣기
  # 박스플롯 색상 변경
  Final.list = Test___MeanDiff___Single.Response___Final.Results(save.path,
                                                                 filename,
                                                                 Norm.Test_combined.df,
                                                                 Equal.Var.Test_combined.df,
                                                                 Mean.Diff.Results)
}
## 플롯에서 샘플사이즈 표시 방법
## meandiff 결과에서 샘플 사이즈
















