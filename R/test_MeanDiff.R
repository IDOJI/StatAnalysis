test_MeanDiff = function(X,
                     group,
                     variable,
                     round.digits=100,
                     norm_alpha=0.05,
                     anova_alpha=0.05,
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
  results_NormHomo.list = test_Group_Normality_and_Homoscedasticity(X, group, variable, norm_alpha)
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
  MeanDiff_Plotting = function(results.subtitle=T, pairwise.comparisons=T, ...){
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
      pairwise.comparisons = pairwise.comparisons,                    # Adjustment method for p-values for multiple comparisons.
      pairwise.annotation = "p.value",                # how do you want to annotate the pairwise comparisons
      p.adjust.method = p.adjust.method,              # method for adjusting p-values for multiple comparisons
      pairwise.display = "significant",
      conf.level = 1-anova_alpha,




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
  p = MeanDiff_Plotting(results.subtitle, pairwise.comparisons = T)




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

    # post-hoc alpha
    which_posthoc_sig = which(MeanDiff.df$PostHoc_p.val <= posthoc_alpha)
    comparisons = list()
    if(length(which_posthoc_sig)>0){
      for(i in 1:length(which_posthoc_sig)){
        comparisons[[i]] = MeanDiff.df[which_posthoc_sig[i],c("Group_1", "Group_2")]
      }
    }
    if(length(comparisons)==0){
      p = MeanDiff_Plotting(results.subtitle, pairwise.comparisons = F)
    }else{
      p = MeanDiff_Plotting(results.subtitle, pairwise.comparisons = T)
      # if(is.normal){
      #   # p = p + ggsignif::geom_signif(comparisons = comparisons,
      #   #                               test = "t.test",
      #   #                               map_signif_level = TRUE,
      #   #                               col = 2,
      #   #                               size = 1,
      #   #                               annotations = c("***")) %>% suppressWarnings()
      # }
      if(M_results.df$MeanDiff_p.val[1] <= anova_alpha){
        # Saving images
        ggplot2::ggsave(filename = filename, plot = p, path = path, units="px", dpi = 300, limitsize = F)
      }

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
#
ggbetweenstats = function (data, x, y, plot.type = "boxviolin", type = "parametric",
                           pairwise.comparisons = TRUE, pairwise.display = "significant",
                           p.adjust.method = "holm", effsize.type = "unbiased", bf.prior = 0.707,
                           bf.message = TRUE, results.subtitle = TRUE, xlab = NULL,
                           ylab = NULL, caption = NULL, title = NULL, subtitle = NULL,
                           k = 2L, var.equal = FALSE, conf.level = 0.95, nboot = 100L,
                           tr = 0.2, centrality.plotting = TRUE, centrality.type = type,
                           centrality.point.args = list(size = 5, color = "darkred"),
                           centrality.label.args = list(size = 3, nudge_x = 0.4, segment.linetype = 4,
                                                        min.segment.length = 0), outlier.tagging = FALSE, outlier.label = NULL,
                           outlier.coef = 1.5, outlier.shape = 19, outlier.color = "black",
                           outlier.label.args = list(size = 3), point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                                                                                  alpha = 0.4, size = 3, stroke = 0), violin.args = list(width = 0.5,
                                                                                                                                         alpha = 0.2), ggsignif.args = list(textsize = 3, tip_length = 0.01),
                           ggtheme = ggstatsplot::theme_ggstatsplot(), package = "RColorBrewer",
                           palette = "Dark2", ggplot.component = NULL, output = "plot",
                           ...)
{

  type <- stats_type_switch(type)
  c(x, y) %<-% c(ensym(x), ensym(y))
  outlier.label <- if (!quo_is_null(enquo(outlier.label)))
    ensym(outlier.label)
  data %<>% select({
    {
      x
    }
  }, {
    {
      y
    }
  }, outlier.label = {
    {
      outlier.label
    }
  }) %>% tidyr::drop_na(.) %>% mutate(`:=`({
    {
      x
    }
  }, droplevels(as.factor({
    {
      x
    }
  }))))
  if (!"outlier.label" %in% names(data))
    data %<>% mutate(outlier.label = {
      {
        y
      }
    })
  data %<>% .outlier_df(x = {
    {
      x
    }
  }, y = {
    {
      y
    }
  }, outlier.coef = outlier.coef, outlier.label = outlier.label)
  test <- ifelse(nlevels(data %>% pull({
    {
      x
    }
  })) < 3, "t", "anova")
  if (results.subtitle) {
    .f.args <- list(data = data, x = as_string(x), y = as_string(y),
                    effsize.type = effsize.type, conf.level = conf.level,
                    var.equal = var.equal, k = k, tr = tr, paired = FALSE,
                    bf.prior = bf.prior, nboot = nboot)
    .f <- .f_switch(test)
    subtitle_df <- .eval_f(.f, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df))
      subtitle_df$expression[[1]]
    if (type == "parametric" && bf.message) {
      caption_df <- .eval_f(.f, !!!.f.args, type = "bayes")
      caption <- if (!is.null(caption_df))
        caption_df$expression[[1]]
    }
  }
  if (output != "plot") {
    return(switch(output, caption = caption, subtitle))
  }
  plot <- ggplot(data, mapping = aes({
    {
      x
    }
  }, {
    {
      y
    }
  })) + exec(geom_point, data = ~filter(.x, !isanoutlier),
             aes(color = {
               {
                 x
               }
             }), !!!point.args)
  if (isFALSE(outlier.tagging)) {
    plot <- plot + exec(geom_point, data = ~filter(.x, isanoutlier),
                        aes(color = {
                          {
                            x
                          }
                        }), !!!point.args)
  }
  if (plot.type == "violin" && isTRUE(outlier.tagging)) {
    plot <- plot + geom_point(data = ~filter(.x, isanoutlier),
                              size = 3, stroke = 0, alpha = 0.7, color = outlier.color,
                              shape = outlier.shape)
  }
  if (plot.type %in% c("box", "boxviolin")) {
    if (isTRUE(outlier.tagging)) {
      .f <- stat_boxplot
      outlier_list <- list(outlier.shape = outlier.shape,
                           outlier.size = 3, outlier.alpha = 0.7, outlier.color = outlier.color)
    }
    else {
      .f <- geom_boxplot
      outlier_list <- list(outlier.shape = NA, position = position_dodge(width = NULL))
    }
    suppressWarnings(plot <- plot + exec(.fn = .f, width = 0.3,
                                         alpha = 0.2, geom = "boxplot", coef = outlier.coef,
                                         !!!outlier_list))
  }
  if (plot.type %in% c("violin", "boxviolin")) {
    plot <- plot + exec(geom_violin, !!!violin.args)
  }
  if (isTRUE(outlier.tagging)) {
    plot <- plot + exec(.fn = ggrepel::geom_label_repel,
                        data = ~filter(.x, isanoutlier), mapping = aes(x = {
                          {
                            x
                          }
                        }, y = {
                          {
                            y
                          }
                        }, label = outlier.label), min.segment.length = 0,
                        inherit.aes = FALSE, !!!outlier.label.args)
  }
  if (isTRUE(centrality.plotting)) {
    plot <- .centrality_ggrepel(plot = plot, data = data,
                                x = {
                                  {
                                    x
                                  }
                                }, y = {
                                  {
                                    y
                                  }
                                }, k = k, type = stats_type_switch(centrality.type),
                                tr = tr, centrality.point.args = centrality.point.args,
                                centrality.label.args = centrality.label.args)
  }
  if (isTRUE(pairwise.comparisons) && test == "anova") {
    mpc_df <- pairwise_comparisons(data = data, x = {
      {
        x
      }
    }, y = {
      {
        y
      }
    }, type = type, tr = tr, paired = FALSE, var.equal = var.equal,
    p.adjust.method = p.adjust.method, k = k)
    plot <- .ggsignif_adder(plot = plot, mpc_df = mpc_df,
                            data = data, x = {
                              {
                                x
                              }
                            }, y = {
                              {
                                y
                              }
                            }, pairwise.display = pairwise.display, ggsignif.args = ggsignif.args) + ggsignif::geom_signif(comparisons       =  selected_groups,
                                                                                                                           map_signif_level  =  T,
                                                                                                                           step_increase     =  0.1)
    seclabel <- .pairwise_seclabel(unique(mpc_df$test), ifelse(type ==
                                                                 "bayes", "all", pairwise.display))
  }
  else {
    seclabel <- NULL
  }
  .aesthetic_addon(plot = plot, x = data %>% pull({
    {
      x
    }
  }), xlab = xlab %||% as_name(x), ylab = ylab %||% as_name(y),
  title = title, subtitle = subtitle, caption = caption,
  seclabel = seclabel, ggtheme = ggtheme, package = package,
  palette = palette, ggplot.component = ggplot.component)
}






pairwise_comparisons = function (data, x, y,
                                 subject.id = NULL, type = "parametric",
                                 paired = FALSE,
                                 var.equal = FALSE, tr = 0.2,
                                 bf.prior = 0.707,
                                 p.adjust.method = "holm", k = 2L, ...)
{
  type <- stats_type_switch(type)
  if (type != "robust")
    check_if_installed("PMCMRplus", reason = "for pairwise comparisons")
  c(x, y) %<-% c(ensym(x), ensym(y))
  data %<>% long_to_wide_converter(x = {
    {
      x
    }
  }, y = {
    {
      y
    }
  }, subject.id = {
    {
      subject.id
    }
  }, paired = paired, spread = FALSE)
  x_vec <- pull(data, {
    {
      x
    }
  })
  y_vec <- pull(data, {
    {
      y
    }
  })
  g_vec <- pull(data, .rowid)
  .f.args <- list(...)
  if (type %in% c("parametric", "bayes")) {
    if (var.equal || paired)
      c(.f, test) %<-% c(stats::pairwise.t.test, "Student's t")
    if (!(var.equal || paired))
      c(.f, test) %<-% c(PMCMRplus::gamesHowellTest, "Games-Howell")
  }
  if (type == "nonparametric") {
    if (!paired)
      c(.f, test) %<-% c(PMCMRplus::kwAllPairsDunnTest,
                         "Dunn")
    if (paired)
      c(.f, test) %<-% c(PMCMRplus::durbinAllPairsTest,
                         "Durbin-Conover")
    .f.args <- list(y = y_vec, ...)
  }
  if (type != "robust") {
    df <- suppressWarnings(exec(.fn = .f, x = y_vec, g = x_vec,
                                groups = x_vec, blocks = g_vec, paired = paired,
                                p.adjust.method = "none", !!!.f.args)) %>% tidy_model_parameters() %>%
      rename(group2 = group1, group1 = group2)
  }
  if (type == "robust") {
    if (!paired) {
      c(.ns, .fn) %<-% c("WRS2", "lincon")
      .f.args <- list(formula = new_formula(y, x), data = data,
                      method = "none")
    }
    if (paired) {
      c(.ns, .fn) %<-% c("WRS2", "rmmcp")
      .f.args <- list(y = quote(y_vec), groups = quote(x_vec),
                      blocks = quote(g_vec))
    }
    df <- eval(call2(.ns = .ns, .fn = .fn, tr = tr, !!!.f.args)) %>%
      tidy_model_parameters()
    test <- "Yuen's trimmed means"
  }
  if (type == "bayes") {
    df_tidy <- purrr::map_dfr(.x = purrr::map2(.x = as.character(df$group1),
                                               .y = as.character(df$group2), .f = function(a, b) droplevels(filter(data,
                                                                                                                   {
                                                                                                                     {
                                                                                                                       x
                                                                                                                     }
                                                                                                                   } %in% c(a, b)))), .f = ~two_sample_test(data = .x,
                                                                                                                                                            x = {
                                                                                                                                                              {
                                                                                                                                                                x
                                                                                                                                                              }
                                                                                                                                                            }, y = {
                                                                                                                                                              {
                                                                                                                                                                y
                                                                                                                                                              }
                                                                                                                                                            }, paired = paired, bf.prior = bf.prior, type = "bayes")) %>%
      filter(term == "Difference") %>% mutate(expression = glue("list(log[e]*(BF['01'])=='{format_value(-log(bf10), k)}')")) %>%
      mutate(test = "Student's t")
    df <- bind_cols(select(df, group1, group2), df_tidy)
  }
  df %<>% mutate_if(.predicate = is.factor, .funs = ~as.character()) %>%
    arrange(group1, group2) %>% select(group1, group2, everything())
  if (type != "bayes") {
    df %<>% mutate(p.value = stats::p.adjust(p = p.value,
                                             method = p.adjust.method), p.adjust.method = p_adjust_text(p.adjust.method),
                   test = test) %>% mutate(expression = case_when(p.adjust.method ==
                                                                    "None" ~ glue("list(italic(p)[unadj.]=='{format_value(p.value, k)}')"),
                                                                  TRUE ~ glue("list(italic(p)['{p.adjust.method}'-adj.]=='{format_value(p.value, k)}')")))
  }
  select(df, everything(), -matches("p.adjustment|^method$")) %>%
    .glue_to_expression() %>% as_tibble()
}



