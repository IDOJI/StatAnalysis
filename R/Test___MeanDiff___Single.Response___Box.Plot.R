Test___MeanDiff___Single.Response___Box.Plot = function(df, var_group, var_response, Mean.Diff.Results, alpha_ANOVA=NULL, alpha_PostHoc=NULL, palette="lancet"){
  ############################################################################
  # Finding significant comparing groups
  ############################################################################
  Mean.Diff = Mean.Diff.Results[[1]]
  which_signif = which(unlist(Mean.Diff$MeanDiff_p.value) <= alpha_ANOVA)
  Mean.Diff_Signif = Mean.Diff[which_signif, ]
  names(Mean.Diff_Signif)

  Post.Hoc  = Mean.Diff.Results[[2]]
  if(!is.null(Post.Hoc)){
    Post.Hoc_Signif = Post.Hoc[Post.Hoc$PostHoc_p.value_adj <= alpha_PostHoc, ]
    Post.Hoc_Signif_tibble = Post.Hoc_Signif %>% dplyr::select(group1, group2, PostHoc_p.value_adj, PostHoc_p.value.signif)
  }






  ############################################################################
  # Boxplot
  ############################################################################
  p1 = ggpubr::ggboxplot(data = df,
                         x = var_group,
                         y = var_response,
                         color = var_group,
                         palette = "lancet",
                         shape = var_group,
                         size = 0.3,
                         add = "jitter",
                         add.params = list(size=0.5))




  ############################################################################
  # Label bold
  ############################################################################
  p2 = p1 + ggpubr::font("xlab", size = 18, face = "bold") + ggpubr::font("ylab", size = 18, face = "bold")




  ############################################################################
  # Adding p-values on comparing groups
  ############################################################################
  if(is.null(Post.Hoc) && nrow(Mean.Diff_Signif)>0){
    # 두 그룹 사이의 비교 옵션 넣기
    group = df[,var_group] %>% unique %>% unlist
    Mean.Diff_Signif$group1 = group[1]
    Mean.Diff_Signif$group2 = group[2]

    p3 = p2 + ggpubr::stat_pvalue_manual(Mean.Diff_Signif,
                                         y.position = 1.1*max(df[,var_response] %>% unlist %>% as.numeric),
                                         step.increase = 0.1,
                                         label = "MeanDiff_p.value.signif",
                                         label.size = 5,
                                         bracket.size = 0.8)
  }else{
    p3 = p2 + ggpubr::stat_pvalue_manual(Post.Hoc_Signif_tibble,
                                         y.position = 1.1*max(df[,var_response] %>% unlist %>% as.numeric),
                                         step.increase = 0.1,
                                         label = "PostHoc_p.value.signif",
                                         label.size = 5,
                                         bracket.size = 0.8)
  }




  ############################################################################
  # Adding sample size
  ############################################################################
  p4 = p3 + EnvStats::stat_n_text(text.box = F, size = 4)




  ############################################################################
  # Change font size
  ############################################################################
  p5 = p4 + theme(text = element_text(size = 10)) # change text size of theme components


  #ggsave("test.png", plot = p5, path = path)
  return(p5)
}

#
# ############################################################################
# # Creating Background of a plot
# ############################################################################
# p1 = ggpubr::ggboxplot(data    = df,
#                        x       = var_group,
#                        y       = var_response,
#                        color   = var_group,
#                        #palette = ,
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

# ############################################################################
# # Palette
# ############################################################################
#
# Plot___N.Palette = function(n){
#   if(n==3){
#     palette = c("#00AFBB", "#E7B800", "#FC4E07")
#   }if(n==4){
#     palette = c("#00AFBB", "#E7B800", "#FC4E07", )
#   }else{
#     palette = NULL
#   }
#   return(palette)
# }
