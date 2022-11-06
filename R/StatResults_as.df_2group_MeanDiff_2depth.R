StatResults_as.df_2group_MeanDiff_2depth = function(results.list){
  ### create a df
  results.df = matrix(" ", nrow=length(results.list), ncol=3) %>% as.data.frame

  ### Group
  results.df[1,1] = names(results.list[[1]][[1]])[1];names(results.df)[1]="Group_1"
  results.df[1,2] = names(results.list[[1]][[1]])[2];names(results.df)[2]="Group_2"

  ### Variables used
  results.df[,3] = names(results.list);names(results.df)[3] = "Variables"

  ### naming
  names(results.df)
  combined.list = lapply(results.list, FUN=function(x){
    # x = results.list[[1]]
    x = unlist(x)

    # change name
    mean_diff = names(x)[4]
    mean_diff_type = substr(mean_diff, nchar("Mean_difference")+2, nchar(mean_diff)-1)
    x = c(x, mean_diff_type)
    names(x)[length(x)-1] = "Mean_diff_p.val"
    names(x)[length(x)] = "Mean_diff_type"

    x.df = matrix(x, nrow=1) %>% as.data.frame
    names(x.df) = names(x)
    x.df[,1:4] = x.df[,1:4] %>% as.numeric
    return(x.df)
  })

  ### as data frame
  combined.df = do.call(rbind, combined.list)

  ### returning
  return(cbind(results.df, combined.df))
}
