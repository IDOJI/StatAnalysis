Test___Corr___Multi = function(data.df, X, Y, group=NULL,
                               method=c("pearson", "kendall", "spearman"),
                               alpha=0.05,
                               save.path,
                               expression=F){
  ### create path
  dir.create(save.path, showWarnings = F)


  ### Extracting & Plotting results
  results.list_2 = lapply(Y, FUN=function(y, ...){
    results.list_1 = lapply(X, FUN=function(x,...){
      file.name = paste0("[", "Correlation","]","`",x,"`", "_","`", y,"`")
      return(Test___Corr(data.df, x, y, group, method, alpha, save.path, file.name, expression))

    })
    return(do.call(rbind, results.list_1))
  })



  #=========================================================================여기까지 정리 완료
  ### exporting exch xlsx
  lapply(results.list_2, FUN=function(ith.df, ...){
    # ith.df = results.list_2[[1]]
    filename = paste0("[", "Correlation Test","] ", "x", "  vs  ", "`", unique(ith.df$y), "`")
    Test___Corr___Exporting.XLSX.Highlight(Corr.Test_results.df = ith.df, alpha, save.path, filename)
  })


  return(results.list_2)
}
