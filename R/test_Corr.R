test_Corr = function(data.df, x, y, type = "parametric", alpha=0.05, group=NULL, path=NULL, filename=NULL){
  if(type=="parametric"){
    results = cor.test(data.df[,x] %>% unlist, data.df[,y] %>% unlist, method="pearson", conf.level=1-alpha)
    Method = "Pearson"
  }

  if(results$p.value <= alpha){
    is.sig = "*"
  }else{
    is.sig = ""
  }
  results.df = data.frame(X=x, Y=y, Method=Method, Estimate=results$estimate, p.val=results$p.value, Significant=is.sig)
  names(results.df)[ncol(results.df)] = paste("Significant(a=", alpha, ")", sep="")

  if(!is.null(path) && !is.null(filename)){
    if(is.null(group)){
      Plotting_Correlation(data.df, x, y, method="pearson", path=path, alpha=alpha, file.name = filename)
    }else{
      Plotting_Correlation(data.df, x, y, group, method="pearson", path=path, alpha=alpha, file.name = filename)
    }
  }
  return(results.df)
}
