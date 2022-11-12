summary_prop = function(data.df, demo.col){
  # demo.col = "Sex"
  results.df = matrix(NA, 1, length(demo.col)) %>% as.data.frame
  names(results.df) = demo.col
  for(i in 1:length(demo.col)){
    prop = data.df[,demo.col[i]] %>% unlist %>% as.factor %>% table
    percentage = prop %>% prop.table %>% round(4)
    results.df[1,i] = paste(paste(names(prop), " : ", prop, " (", percentage, "%)", sep=""), collapse = " / ")
  }
  return(results.df)
}
