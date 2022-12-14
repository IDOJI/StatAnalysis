Test___Corr = function(data.df, x, y, group=NULL, mothod=c("pearson", "kendall", "spearman"),alpha=0.05, save.path=NULL, file.name, expression=F){
  ### corr test & plotting
  Corr.Test_results.list = Test___Corr___Plotting.Scatter(data.df, x, y, group,
                                                          method, alpha,
                                                          save.path, file.name,
                                                          expression = F)
  ### Extract = corr test results
  Extracted_results = Test___Corr___Extract.Results(Corr.Test_results.list)
  return(Extracted_results)
}

# Corr.Test_results = cor.test(data.df[,x] %>% unlist, data.df[,y] %>% unlist, method=method, conf.level=1-alpha)
# Extracted_results = Test___Corr___Extract.Results(Corr.Test_results, alpha, x, y)
