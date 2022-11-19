test_MeanDiff = function(X,
                     group,
                     variable,
                     round.digits=100,
                     alpha=0.05,
                     posthoc_alpha=0.05,
                     p.adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                     outlier.label=NULL,
                     title="",
                     results.subtitle=T,
                     path,
                     filename=c(".png", ".pdf"),
                     exclude_rows=NULL,
                     ex_reasons=NULL){
  #============================================================================
  # Normality & Homoscedasticity
  #============================================================================
  results_NormHomo.list = test_Group_Normality_and_Homoscedasticity(X, group, variable, alpha)
  is.normal = results_NormHomo.list$is.norm
  is.homo = results_NormHomo.list$is.homo
  is.balanced = results_NormHomo.list$is.balanced



  #============================================================================
  # Plotting arguments by the criteria
  #============================================================================
  # type = c("parametric", "p", "robust")
  if(is.normal){
    type = "parametric"
  }else{
    type = "nonparametric"
  }



  #============================================================================
  # Asterisk label
  #============================================================================
  # df = pairwise_comparisons(data.df, !!group, !!variable) %>%
  #   dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  #   dplyr::arrange(group1) %>%
  #   dplyr::mutate(asterisk_label = c("**", "***", "**"))



  #============================================================================
  # Meandiff test with Plotting
  #============================================================================
  MeanDiff_plotting = function(results.subtitle, ...){
    p = ggstatsplot::ggbetweenstats(
      data = X,
      x = !!group,
      y = !!variable,
      #grouping.var = genre,                           # grouping variable
      k = 4,                                          # number of decimal places for statistical results


      # test type @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      type = type,                                    # which type of test is to be run
      var.equal = is.homo,



      # outlier & mean @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      outlier.tagging = TRUE,                         # whether outliers need to be tagged
      #outlier.label = !!outlier.label,                # variable to be used for the outlier tag
      outlier.label.color = "darkgreen",              # changing the color for the text label
      mean.plotting = TRUE,                           # whether mean for each group is to be displayed
      # #mean.ci = TRUE,                                # whether to display confidence interval for means
      mean.label.size = 2.5,                          # size of the label for mean




      # pairwise @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      pairwise.comparisons = TRUE,                    # Adjustment method for p-values for multiple comparisons.
      pairwise.annotation = "p.value",                # how do you want to annotate the pairwise comparisons
      p.adjust.method = p.adjust.method,              # method for adjusting p-values for multiple comparisons
      pairwise.display = "significant",
      conf.level = 1-alpha,




      # plot @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # xlab = xlab,                                    # label for the x-axis variable
      # ylab = ylab,                                    # label for the y-axis variable
      title = title,                                  # title text for the plot
      centrality.label.args = list(nudge_x = -0.4),
      ggplot.component = list(ggplot2::theme(plot.title = element_text(size = 20, face = "bold"))),
      notch = FALSE,                                    # show notched box plot
      bf.message = TRUE,                             # add a message with bayes factor favoring null
      # ggtheme = ggthemes::theme_fivethirtyeight(),    # choosing a different theme
      # ggstatsplot.layer = FALSE,                      # turn off ggstatsplot theme layer
      # package = "wesanderson",                        # package from which color palette is to be taken
      # palette = "Darjeeling1",                        # choosing a different color palette
      messages = FALSE,


      # returning statistical results  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      results.subtitle=results.subtitle                           #  Setting it to FALSE will return only the plot.
    )
    return(p)
  }
  p = MeanDiff_plotting(results.subtitle=T)


  #============================================================================
  # Mean test results
  #============================================================================
  p_results.list = ggstatsplot::extract_stats(p)
  M = p_results.list[[1]]
  M_results.df = data.frame(Response                     =   variable,
                            MeanDiff_TestType            =   M$method,
                            MeanDiff_p.val               =   M$p.value)
  if(!is.null(p_results.list[[3]])){
    pair = p_results.list[[3]]
    pair_results.df = data.frame(PostHoc_TestType        =   pair$test,
                                 Group_1                 =   pair$group1,
                                 Group_2                 =   pair$group2,
                                 PostHoc_p.val           =   pair$p.value,
                                 PostHoc_p.adjust.method =   pair$p.adjust.method)
    MeanDiff.df = ccbind(M_results.df, pair_results.df)

    # Saving images
    if(sum(pair_results.df$PostHoc_p.val <= posthoc_alpha)>0){
      p = MeanDiff_plotting(results.subtitle=F)
      ggplot2::ggsave(filename = filename, plot = p, path = path, units="px", dpi = 300, limitsize = F)
    }
  }else{
    MeanDiff.df = M_results.df
  }



  #============================================================================
  # final return
  #============================================================================
  final.df = ccbind(results_NormHomo.list[[1]], MeanDiff.df)
  return(final.df)
}
















# #============================================================================
# # Excluding rows
# #============================================================================
# # ex_reasons = "Small Sample Size"
# if(!is.null(exclude_rows)){
#   excluded = X[exclude_rows,group] %>% unlist
#   X = X[-exclude_rows,]
# }

# ### excluding vec & response col
# response_col_1 = c(variable, rep(" ", n_group-1))
# if(is.null(exclude_rows)){
#   excluded.vec = rep(" ", length(response_col_1))
# }else{
#   if(is.null(ex_reasons)){
#     bec = NULL
#   }else{
#     bec = "∵"
#   }
#   excluded.vec = c(c(exclude_rows, excluded, paste("(", bec, ex_reasons, ")", sep="")),
#                    rep(" ", length(response_col_1)-3))
# }







#
#
# ### Criteria
# ### Parametric @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ## norm O / homo O
# c1 = is.balanced && is.normal && is.homo # One-way ANOVA(Balanced)
# c2 = !is.balanced && is.normal && is.homo # One-way ANOVA(Unbalanced)
# ## norm O / homo X
# c3 = is.balanced && is.normal && !is.homo # balance O
# c4 = !is.balanced && is.normal && !is.homo # balance X
# ### non-Parametric @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ## norm X
# c5 = is.balanced && !is.normal
# c6 = !is.balanced && !is.normal

