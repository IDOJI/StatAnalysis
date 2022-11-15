Quest_InvCoding = function(data.df, which.col=c(2,5,6), score=c("1","2.25","3.5","4.75","6")){
  # data.df = dataset_ExNA[,first:(first+35)]
  score = as.character(score)
  which_ind.list = list()

  # as list each ith_col
  data.list = lapply(which.col, data.df, FUN=function(x, data.df){
    y = data.df[,x] %>% unlist
    names(y) = NULL
    return(y)
  })

  # change each col
  results.list = lapply(data.list, score, FUN=function(x, score){
    # x = data.list[[1]]
    is_numeric = is.numeric(x)
    x = as.character(x)
    reversed = sapply(x, score, FUN=function(y,score){
      # y= x[1]
      rev_score = rev(score)
      return(rev_score[which(score==y)])
    })
    if(is_numeric){
      return(reversed %>% as.numeric)
    }else{
      return(reversed)
    }
  })
  results.df = do.call(cbind, results.list) %>% as.data.frame
  names(results.df) = names(data.df)[which.col]
  data.df[,which.col] = results.df

  return(data.df)
}
