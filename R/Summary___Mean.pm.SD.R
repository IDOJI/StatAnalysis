Summary___Mean.pm.SD = function(data.df, demo.col){
  pm_results = c()
  for(i in 1:length(demo.col)){
    # i=1
    ith_col = data.df[,demo.col[i]] %>% unlist
    pm_results = c(pm_results, paste(round(mean(ith_col),4), round(sd(ith_col),4), sep=" ± "))
  }
  names(pm_results) = demo.col
  return(pm_results)
}
