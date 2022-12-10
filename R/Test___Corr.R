Test___Corr = function(data.df, x, y, mothod=c("pearson", "kendall", "spearman"),alpha=0.05, group=NULL, path=NULL, filename=NULL){
  results = cor.test(data.df[,x] %>% unlist, data.df[,y] %>% unlist, method=method, conf.level=1-alpha)
  if(results$p.value <= alpha){
    is.sig = "*"
  }else{
    is.sig = ""
  }
  results.df = data.frame(X=x, Y=y, Method=method, Estimate=results$estimate, p.val=results$p.value, Significant=is.sig)
  names(results.df)[ncol(results.df)] = paste("Significant(a=", alpha, ")", sep="")

  if(!is.null(path) && !is.null(filename)){
    if(is.null(group)){
      Plotting___Correlation(data.df, x, y, method=method, path=path, alpha=alpha, file.name = filename)
    }else{
      Plotting___Correlation(data.df, x, y, group, method=method, path=path, alpha=alpha, file.name = filename)
    }
  }
  return(results.df)
}
