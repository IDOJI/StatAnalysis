Test___MeanDiff___Single.Response___ggstats = function(# data & variables
                                                    df,
                                                    var_group,
                                                    var_response,
                                                    alpha_ANOVA,
                                                    # test results
                                                    is.Normal,
                                                    is.Equal.Var,
                                                    # plotting options
                                                    results.subtitle=F,
                                                    pairwise.comparisons=T,
                                                    p.adjust.method,
                                                    title=""){
  if(is.Normal){
    type = "parametric"
  }else{
    type = "nonparametric"
  }


  p = ggstatsplot::ggbetweenstats(
    data = df,
    x = !!var_group,
    y = !!var_response,
    #grouping.var = genre,                           # grouping variable
    k = 4,                                          # number of decimal places for statistical results


    # test type @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    type = type,                                    # which type of test is to be run
    var.equal = is.Equal.Var,


    # outlier & mean @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    outlier.tagging = FALSE,                         # whether outliers need to be tagged
    #outlier.label = !!outlier.label,                # variable to be used for the outlier tag
    #outlier.label.color = "darkgreen",              # changing the color for the text label
    mean.plotting = TRUE,                           # whether mean for each group is to be displayed
    # #mean.ci = TRUE,                                # whether to display confidence interval for means
    mean.label.size = 2.5,                          # size of the label for mean


    # pairwise @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    pairwise.comparisons = pairwise.comparisons,                    # Adjustment method for p-values for multiple comparisons.
    pairwise.annotation = "p.value",                # how do you want to annotate the pairwise comparisons
    p.adjust.method = p.adjust.method,              # method for adjusting p-values for multiple comparisons
    pairwise.display = "significant",
    conf.level = 1-alpha_ANOVA,


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


#============================================================================
# Plotting arguments by the criteria
#============================================================================
# type = c("parametric", "p", "robust")



#============================================================================
# Asterisk label
#============================================================================
# df = pairwise_comparisons(data.df, !!group, !!variable) %>%
#   dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
#   dplyr::arrange(group1) %>%
#   dplyr::mutate(asterisk_label = c("**", "***", "**"))

