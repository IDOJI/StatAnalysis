test_MeanDiff_Export = function(ANOVA_results.list,
                                save_path,
                                file.name){
  #=============================================================================
  # Dividing each element
  #=============================================================================
  ANOVA_results = ANOVA_results.list[[1]]
  alpha = ANOVA_results.list[[2]]
  alpha_adjusted = ANOVA_results.list[[3]]
  alpha_adjusted_posthoc = ANOVA_results.list[[4]]



  #=============================================================================
  # Exporting Colored excels
  #=============================================================================
  which_MeanDiff_sig = which(ANOVA_results$MeanDiff_p.val<=alpha_adjusted)
  which_PostHoc_sig = which(ANOVA_results$PostHoc_p.val<=alpha_adjusted_posthoc)
  colors.list = list("red", "#F7FE2E", "#FE9A2E", "#F7FE2E")
  which_cols.list = which_cols(ANOVA_results, which.cols=c("Response", "MeanDiff_p.val", "PostHoc_Groups", "PostHoc_p.val")) %>% as.list
  coloring_index.list = list(which_MeanDiff_sig, which_MeanDiff_sig, which_PostHoc_sig, which_PostHoc_sig)
  coloring_xlsx_cells(ANOVA_results, colors.list, which_cols.list, coloring_index.list, save_path, file.name)



  #=============================================================================
  # Exporting Boxplots for significant cases
  #=============================================================================



}

























#
# if(length(ith_ANOVA_results)==3){
#   # exporting colored excel file
#   MD = ith_ANOVA_results[[1]]
#   MC = ith_ANOVA_results[[2]]
#   results_table = ith_ANOVA_results[[3]]
#
#
#   if(!is.list(MC) && is.data.frame(MC)){
#     MC.list = list(MC)
#   }
#
#   ### Exporting Results of MeanDiff
#   # test_MeanDiff_Export_coloring(MD, file_name = excel_file_name, save_path = save_path, alpha=alpha, colors.list=colors.list)
#
#
#   ### Exporting Results Table of MeanDiff & PostHoc
#
#
#
#   ### Exporting PostHoc Results
#   for(k in 1:length(MC)){
#     ### k=1
#     kth_MC = MC[[k]]
#     response = kth_MC$Response[1]
#     kth_excel_file_name = paste(excel_file_name, "_PostHoc","_(",response,")", sep="")
#     kth_plot_file_name = paste(kth_excel_file_name, "Boxplot", sep="_")
#     # test_MeanDiff_Export_coloring(kth_MC, file_name = kth_excel_file_name, save_path = save_path, alpha=alpha, colors.list=colors.list)
#
#     ### exporting boxplot
#     test_MeanDiff_Export_boxplot(data.df = data.df,
#                                  MeanDiff_results.df = kth_MC,
#                                  alpha = alpha,
#                                  save.path = save_path,
#                                  filename = kth_plot_file_name)
#   }
# }else{
#   MD = ith_ANOVA_results[[1]]
#   results_table = ith_ANOVA_results[[2]] %>% as.data.frame
#   # test_MeanDiff_Export_coloring(MD, file_name = file_name, save_path = save_path, alpha=alpha, colors.list=colors.list)
#   test_MeanDiff_Export_coloring(results_table, file_name = excel_file_name, save_path = save_path, alpha=alpha, colors.list=colors.list)
# }
# getwd()
#
# p = ggbetweenstats(
#   data = dataset.df,
#   x = "AHI_group",
#   y = "RMI Index N-S",
#   type = "nonparametric", # ANOVA or Kruskal-Wallis
#   var.equal = F, # ANOVA or Welch ANOVA
#   plot.type = "box",
#   pairwise.comparisons = TRUE,
#   pairwise.display = "significant",
#   centrality.plotting = FALSE,
#   bf.message = FALSE
# )
# p
#
# method1 <- "anova"
# method2 = "t.test"
#
# require(ggpubr)
# p = ggboxplot(dataset.df,
#              x = "AHI_group", y = "RMI Index N-S",
#              color = "AHI_group",
#              legend = "none",
#              palette = "npg",
#              add = "jitter")
# my_comparisons <- list(c("Normal","Mild OSA"),c("Normal","Moderate OSA"), c("Normal","Severe OSA"), c("Mild OSA", "Moderate OSA"),c("Mild OSA","Severe OSA"),c("Moderate OSA", "Severe OSA"))
#
# print(
#   p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)), method = method1, label.y = max(dataset.df[, "BMI"], na.rm = TRUE)) +
#     stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
#   )
#
#
#
# ggsave("test.png", plot = p)
#
#
#
# # Welch One way ANOVA test
#
# eval(parse(text="blue")) %>% class
# # Pairwise comparisons (Games-Howell)
# test = rstatix::games_howell_test(dataset.df, formula = as.formula(paste("`","RMI Index N-S", "` ~ ", "AHI_group", sep="")))
#
# # Visualization: box plots with p-values
# pwc2 <- test %>% add_xy_position(x = "AHI_group", step.increase = 1)
# ggboxplot(dataset.df, x = "AHI_group", y = "RMI Index N-S") +
#   stat_pvalue_manual(pwc2, hide.ns = T) +
#   labs(
#     subtitle = get_test_label(stat.test = pwc2, detailed = TRUE),
#     caption = get_pwc_label(pwc2)
#   )
#
#
# stat_pvalue_manual
#
#
# install.packages("ggstatsplot")
# install.packages("PMCMRplus")
# require(ggstatsplot)
# require(PMCMRplus)
#
#
# test_MeanDiff_Export(data.df=dataset.df,
#                      ith_ANOVA_results = ith_ANOVA,
#                      alpha = 0.05,
#                      save_path = path,
#                      excel_file_name = ith_filename)
