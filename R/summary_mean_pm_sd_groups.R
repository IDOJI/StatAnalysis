summary_mean_pm_sd_groups = function(data.df, demo.col, group.col=NULL, group.as.row=F){
  # demo.col = c("Age","AHI")
  # data.df = Data_Grouping_2.df
  # group.col = "AHI_group"

  if(is.null(group.col)){
    # mean_pm_sd
    mean_pm_sd_results = summary_mean_pm_sd(data.df, demo.col)
    return(mean_pm_sd_results)
  }else{
    group.list = as_list_by(data.df, group.col)
    n_group = length(group.list)

    mean_pm_sd_results.list= lapply(group.list, demo.col, group.as.row, FUN=function(x, demo.col=demo.col, group.as.row=group.as.row){
      # x = group.list[[1]]

      x_results = summary_mean_pm_sd(x, demo.col)

      if(group.as.row){
        x_results.df = matrix(NA, 1, length(demo.col)) %>% as.data.frame
        x_results.df[1,] = x_results
        colnames(x_results.df) = demo.col
      }else{
        x_results.df = matrix(NA, length(demo.col), 1) %>% as.data.frame
        x_results.df[,1] = x_results
        rownames(x_results.df) = demo.col
      }
      return(x_results.df)
    })

    if(group.as.row){
      mean_pm_sd_results.df = do.call(rbind, mean_pm_sd_results.list)
      rownames(mean_pm_sd_results.df) = names(group.list)
    }else{
      mean_pm_sd_results.df = do.call(cbind, mean_pm_sd_results.list)
      colnames(mean_pm_sd_results.df) = names(group.list)
    }
    return(mean_pm_sd_results.df)
  }# else
}
