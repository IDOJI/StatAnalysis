summary_prop_groups = function(data.df, demo.col, group.col=NULL, group.as.row=F){
  # demo.col = "Sex"
  # data.df = Data_Grouping_2.df
  # group.col = "AHI_group"

  if(is.null(group.col)){
    # mean_pm_sd
    summary_prop_results.df = summary_prop(data.df, demo.col)
    return(summary_prop_results.df)
  }else{
    group.list = as_list_by(data.df, group.col)

    summary_prop_results.list= lapply(group.list, demo.col, group.as.row, FUN=function(x, demo.col=demo.col, group.as.row=group.as.row){
      # x = group.list[[1]]

      x_results = summary_prop(x, demo.col)

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
      summary_prop_results.df = do.call(rbind, summary_prop_results.list)
      rownames(summary_prop_results.df) = names(group.list)
    }else{
      summary_prop_results.df = do.call(cbind, summary_prop_results.list)
      colnames(summary_prop_results.df) = names(group.list)
    }
    return(summary_prop_results.df)
  }# else
}

#
# summary_prop_groups = function(data.df, demo.col, group.col=NULL, group.as.row=F){
#   # demo.col = "Sex"
#   # data.df = Data_Grouping_2.df
#   # group.col = "AHI_group"
#
#   if(is.null(group.col)){
#     # mean_pm_sd
#     summary_prop_results.df = summary_prop(data.df, demo.col)
#     return(summary_prop_results.df)
#   }else{
#     group.list = as_list_by(data.df, group.col)
#
#     summary_prop_results.list= lapply(group.list, demo.col, group.as.row, FUN=function(x, demo.col=demo.col, group.as.row=group.as.row){
#       # x = group.list[[1]]
#
#       x_results = summary_prop(x, demo.col)
#
#       if(group.as.row){
#         x_results.df = matrix(NA, 1, length(demo.col)) %>% as.data.frame
#         x_results.df[1,] = x_results
#         colnames(x_results.df) = demo.col
#       }else{
#         x_results.df = matrix(NA, length(demo.col), 1) %>% as.data.frame
#         x_results.df[,1] = x_results
#         rownames(x_results.df) = demo.col
#       }
#       return(x_results.df)
#     })
#
#     if(group.as.row){
#       summary_prop_results.df = do.call(rbind, summary_prop_results.list)
#       rownames(summary_prop_results.df) = names(group.list)
#     }else{
#       summary_prop_results.df = do.call(cbind, summary_prop_results.list)
#       colnames(summary_prop_results.df) = names(group.list)
#     }
#     return(summary_prop_results.df)
#   }# else
# }
