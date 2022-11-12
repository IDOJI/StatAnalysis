Plotting_Correlation = function(data.df=data.df, x=x, y=y, group=group, group.levels=group.levels, method=c("pearson", "spearman"), alpha=0.05, path=NULL, file.name=NULL){

  selected.df = data.frame(Group=data.df[,group] ,x=data.df[,x], y=data.df[,y])
  selected.df$Group = factor(selected.df$Group, levels=group.levels)
  results_cor.test = cor.test(selected.df$x, selected.df$y)
  cor_p.val = results_cor.test$p.value
  cor_estimate = results_cor.test$estimate


  neg_col = colorRampPalette(c("#F5A9A9", "red"))
  pos_col = colorRampPalette(c("#81BEF7", "blue"))
  neg_cols = neg_col(100)
  pos_cols = pos_col(100)


  if(cor_estimate>0){
    lm_colour = pos_cols[floor(cor_estimate*100)]
  }else if(cor_estimate<0){
    lm_colour = neg_cols[floor(cor_estimate*100)]
  }else{
    lm_colour = "black"
  }

  # ggplot
  p = ggplot(selected.df, aes(x, y, color=Group))
  # labs
  p = p + labs(x=x, y=y)# title=""
  # change label size
  p = p + theme(axis.title = element_text(size=20, face="bold"),
                title = element_text(size=13, face="bold"))
  # scattering
  p = p + geom_jitter(position = position_jitter(width=.25, height=.25))
  # lm line
  p = p + stat_smooth(method="lm", colour=lm_colour)
  # correlation
  p = p + stat_cor(mapping=aes(x,y), data=selected.df, cor.coef.name="r", method = method,
                   size=13, inherit.aes=F, r.digits = 4, p.digits = 4, p.accuracy = alpha, color=lm_colour)

  return(p)
}
