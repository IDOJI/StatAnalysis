Test___MeanDiff___Single.Response___Box.Plot = function(df, var_group, var_response, Mean.Diff.Results, alpha_PostHoc){
  ############################################################################
  # Finding significant comparing groups
  ############################################################################
  Mean.Diff = Mean.Diff.Results[[1]]
  Post.Hoc  = Mean.Diff.Results[[2]]

  Post.Hoc_Signif = Post.Hoc[Post.Hoc$PostHoc_p.value_adj <= alpha_PostHoc, ]
  Post.Hoc_Signif_tibble = Post.Hoc_Signif %>% dplyr::select(group1, group2, PostHoc_p.value_adj, PostHoc_p.value.signif)



  ############################################################################
  # Boxplot
  ############################################################################
  p1 = ggpubr::ggboxplot(data = df,
                         x = var_group,
                         y = var_response,
                         color = var_group,
                         add = "jitter",
                         shape = var_group)


  ############################################################################
  # Adding p-values on comparing groups
  ############################################################################
  p2 = p1 + ggpubr::stat_pvalue_manual(Post.Hoc_Signif_tibble,
                                       y.position = max(df[,var_response] %>% unlist %>% as.numeric),
                                       step.increase = 0.1,
                                       label = "PostHoc_p.value.signif")
  return(p2)
}

#
# ############################################################################
# # Creating Background of a plot
# ############################################################################
# p1 = ggpubr::ggboxplot(data    = df,
#                        x       = var_group,
#                        y       = var_response,
#                        color   = var_group,
#                        #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#                        add     = "jitter",
#                        shape   = var_group)








# p3 = p2 + ggpubr::stat_compare_means(label.y = 50) # Add global p-value


# # Violin plots with box plots inside
# p3 <- ggviolin(df, x = "dose", y = "len", fill = "dose",
#                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#                add = "boxplot", add.params = list(fill = "white"))+
#   stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
# stat_compare_means(label.y = 50)        # Add global the p-value



#
# p = test_____MeanDiff___Single.Response___ggstats(# data & variables
#   df,
#   var_group,
#   var_response,
#   # test results
#   alpha_ANOVA
#   is.normal,
#   is.Equal.Var,
#   # plotting options
#   title)
# #
#   #############################################################################################
#
#     # post-hoc alpha
#     which_posthoc_sig = which(MeanDiff.df$PostHoc_p.val <= posthoc_alpha)
#     comparisons = list()
#     if(length(which_posthoc_sig)>0){
#       for(i in 1:length(which_posthoc_sig)){
#         comparisons[[i]] = MeanDiff.df[which_posthoc_sig[i],c("Group_1", "Group_2")]
#       }
#     }
#     if(length(comparisons)==0){
#       p = MeanDiff_Plotting(results.subtitle, pairwise.comparisons = F)
#     }else{
#       p = MeanDiff_Plotting(results.subtitle, pairwise.comparisons = T)
#       # if(is.normal){
#       #   # p = p + ggsignif::geom_signif(comparisons = comparisons,
#       #   #                               test = "t.test",
#       #   #                               map_signif_level = TRUE,
#       #   #                               col = 2,
#       #   #                               size = 1,
#       #   #                               annotations = c("***")) %>% suppressWarnings()
#       # }
#       if(M_results.df$MeanDiff_p.val[1] <= anova_alpha){
#         # Saving images
#         ggplot2::ggsave(filename = filename, plot = p, path = path, units="px", dpi = 300, limitsize = T)
#       }
#
#     }
#
#
#   }else{
#     MeanDiff.df = M_results.df
#   }
#
