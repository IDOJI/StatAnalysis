Plot___Correlation = function(data.df, x, y, group=NULL, group.levels=NULL, method=c("pearson", "spearman"), alpha=0.05, path=NULL, file.name=NULL){
  # x=selected_x[15]
  # y=selected_y[4]


  names(data.df)[which_col(data.df, group, exact=T)] = "Group"
  names(data.df)[which_col(data.df, x, exact = T)] = "x"
  names(data.df)[which_col(data.df, y, exact = T)] = "y"
  results_cor.test = cor.test(data.df$x, data.df$y)
  cor_p.val = results_cor.test$p.value
  cor_estimate = results_cor.test$estimate
  neg_col = colorRampPalette(c("#F5A9A9", "red"))
  pos_col = colorRampPalette(c("#81BEF7", "blue"))
  neg_cols = neg_col(10000000)
  pos_cols = pos_col(10000000)


  if(cor_estimate>0){
    lm_colour = pos_cols[floor(abs(cor_estimate)*10000000)]
  }else if(cor_estimate<0){
    lm_colour = neg_cols[floor(abs(cor_estimate)*10000000)]
  }else{
    lm_colour = "black"
  }

  # ggplot
  if(is.null(group)){
    p = ggplot(data=data.df, mapping=aes(x=x, y=y))
  }else{
    p = ggplot(data=data.df, mapping=aes(x=x, y=y, colour=Group))
  }


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
  p = p + stat_cor(mapping=aes(x=x,y=y), data=data.df, cor.coef.name="r", method = method,
                   size=10, inherit.aes=F, r.digits = 4, p.digits = 4, p.accuracy = alpha, color=lm_colour)

  ggsave(paste(file.name, ".png", sep=""), p, path = path, limitsize = T)
}
