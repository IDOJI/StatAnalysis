Test___Corr___Plotting.Scatter = function(data.df, x, y, group=NULL, method=c("pearson", "spearman"), alpha=0.05, save.path=NULL, file.name=NULL, expression=F){
  ### Selecting methods ================================================================================================
  if(method=="pearson"){
    type = "parametric"
  }else if(method=="spearman"){
    type = "nonparametric"
  }


  ### corr test ================================================================================================
  Corr.Test_Results = statsExpressions::corr_test(data.df, x = x, y = y, type = type)



  ### plotting ================================================================================================
  p1 = ggplot2::ggplot(data = data.df, mapping = aes(x=data.df[,x] %>% unlist, y=data.df[,y] %>% unlist))



  ### scatter plot colored by group ================================================================================================
  if(!is.null(group)){
    p2 = p1 + ggplot2::geom_point(aes(color = data.df[,group] %>% unlist))
  }else{
    p2 = p1 + ggplot2::geom_point()
  }


  ### labs ================================================================================================
  p3 = p2 + ggplot2::labs(x=x, y=y, colour=group)# title=""
  p4 = p3 + ggplot2::theme(axis.title = element_text(size=20, face="bold"),
                           title      = element_text(size=13, face="bold"))


  # lm line ===============================================================================================
  p5 = p4 + ggplot2::stat_smooth(method="lm")


  # histogram ===============================================================================================
  p6 = p5 + ggside::geom_xsidehistogram(aes(y = after_stat(count), fill = data.df[,group] %>% unlist), show.legend = F)
  p7 = p6 + ggside::geom_ysidehistogram(aes(x = after_stat(count), fill = data.df[,group] %>% unlist), show.legend = F)


  # correlation ===============================================================================================
  if(expression){
    p8 = p7 + ggplot2::labs(subtitle = Corr.Test_Results$expression[[1]])
  }else{
    p8 = p7
      #ggplot2::labs(subtitle = expression(widehat(italic("r"))[method] == Corr.Test_Results$estimate))
  }

  # save =======================================================================================================
  if(!is.null(save.path)){
    ggsave(paste0(file.name, ".png"), p8, path = save.path)
    cat("\n", crayon::yellow(paste0(file.name, ".png")), crayon::blue("is saved !"),"\n")
  }

  return(list(Corr.Test_results=Corr.Test_Results, Scatter.Plot = p8))
}



#
#
# names(data.df)[which_col(data.df, group, exact=T)] = "Group"
# names(data.df)[which_col(data.df, x, exact = T)] = "x"
# names(data.df)[which_col(data.df, y, exact = T)] = "y"
# results_cor.test = cor.test(data.df$x, data.df$y)
# cor_p.val = results_cor.test$p.value
# cor_estimate = results_cor.test$estimate
# neg_col = colorRampPalette(c("#F5A9A9", "red"))
# pos_col = colorRampPalette(c("#81BEF7", "blue"))
# neg_cols = neg_col(10000000)
# pos_cols = pos_col(10000000)
#
#
# if(cor_estimate>0){
#   lm_colour = pos_cols[floor(abs(cor_estimate)*10000000)]
# }else if(cor_estimate<0){
#   lm_colour = neg_cols[floor(abs(cor_estimate)*10000000)]
# }else{
#   lm_colour = "black"
# }

# # ggplot
# if(is.null(group)){
#   p = ggplot(data=data.df, mapping=aes(x=x, y=y))
# }else{
#   p = ggplot(data=data.df, mapping=aes(x=x, y=y, colour=Group))
# }






