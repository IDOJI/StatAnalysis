test_MeanDiff_Export_With_Chronbach = function(ANOVA_results.list, Chrobach_results, save_path, file.name){
  #=============================================================================
  # Dividing each element
  #=============================================================================
  ANOVA_results = ANOVA_results.list[[1]]
  alpha = ANOVA_results.list[[2]]
  alpha_adjusted = ANOVA_results.list[[3]]
  alpha_adjusted_posthoc = ANOVA_results.list[[4]]




  #=============================================================================
  # Combining Data
  #=============================================================================
  Chronbach = Chronbach_results[,3]
  Chronbach.df = data.frame(Section=names(Chronbach), Chronbach_Alpha=unlist(Chronbach))




  #=============================================================================
  # Exporting Colored excels
  #=============================================================================
  which_MeanDiff_sig = which(ANOVA_results$MeaDiff_p.val<=alpha_adjusted)
  which_PostHoc_sig = which(ANOVA_results$PostHoc_p.val<=alpha_adjusted_posthoc)
  colors.list = list("red", "#F7FE2E", "#FE9A2E", "#F7FE2E")
  which_cols.list = which_cols(ANOVA_results, which.cols=c("Response", "MeaDiff_p.val", "PostHoc_Groups", "PostHoc_p.val")) %>% as.list
  coloring_index.list = list(which_MeanDiff_sig, which_MeanDiff_sig, which_PostHoc_sig, which_PostHoc_sig)
  coloring_xlsx_cells(ANOVA_results, colors.list, which_cols.list, coloring_index.list, save_path, file.name)



  #=============================================================================
  # Exporting Boxplots for significant cases
  #=============================================================================



}
