test_Corr___Multi = function(data.df, X, Y, group=NULL, method=c("pearson", "kendall", "spearman"),alpha=0.05, path, filename="Correlation Results"){
  results.list_2 = lapply(X, FUN=function(x, ...){
    # x= X[1]
    results.list_1 = lapply(Y, FUN=function(y,...){
      # y=Y[1]
      file_name = paste("[",y,"]", "_","[", x,"]", sep="")
      return(test_Corr(data.df, x, y, method, alpha, group, path, filename=file_name))

    })
    return(do.call(rbind, results.list_1))
  })
  results.df = do.call(rbind, results.list_2)


  ### exporting
  Export___xlsx___Highlighting(results.df,
                      colors.list = list("#2E9AFE", "#81F781", "yellow"),
                      which_cols.list = which_cols(results.df, c("X", "Y", "p.val")) %>% as.list,
                      coloring_index.list = rep(list(which(results.df$Significant=="*")),3),
                      save_path = path,
                      file_name = filename)


  return(results.df)
}
