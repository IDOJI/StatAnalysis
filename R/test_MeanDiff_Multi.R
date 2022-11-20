test_MeanDiff_Multi = function(dataset.df, norm_alpha=0.05, anova_alpha ,posthoc_alpha, variables, group_variables, group_filenames, path, p.adjust.method="bonferroni", file_name = "MeanDiff"){
  MeanDiff_results.list = lapply(group_variables, FUN=function(g, ...){
    # g = group_variables[1]
    f = group_filenames[which(g==group_variables)]
    Final.list = lapply(variables, FUN=function(v, ...){
      title = paste(v, "by", g,sep=" ")
      filename = paste(v, "_", f, ".png", sep="")
      test_MeanDiff(X                 =    dataset.df,
                    group             =    g,
                    variable          =    v,
                    norm_alpha        =    norm_alpha,
                    anova_alpha       =    anova_alpha,
                    posthoc_alpha     =    posthoc_alpha,
                    p.adjust.method   =    p.adjust.method,
                    title             =    title,
                    path              =    path,
                    filename          =    filename)
    })
    ### combining
    for(i in 1:length(Final.list)){
      if(i==1){
        Final_results.df = Final.list[[i]]
      }else{
        Final_results.df = rrbind(Final_results.df, Final.list[[i]])
      }
    }
    return(Final_results.df)
  })


  ### combining
  for(i in 1:length(MeanDiff_results.list)){
    if(i==1){
      MeanDiff.df = MeanDiff_results.list[[i]]
    }else{
      MeanDiff.df = rrbind(MeanDiff.df, MeanDiff_results.list[[i]])
    }
  }



  ### highlighting
  which_meandiff_sig = which(MeanDiff.df$MeanDiff_p.val <= anova_alpha)
  which_posthoc_sig = which(MeanDiff.df$PostHoc_p.val <= posthoc_alpha)
  coloring_index.list = c(rep(list(which_meandiff_sig),3),
                          rep(list(which_posthoc_sig),3))
  colors.list = c(rep(list("#F4FA58"), 3),
                  rep(list("#FE9A2E"), 3))
  which_cols.list = which_cols(MeanDiff.df, c("Group Name","Response", "MeanDiff_p.val",
                                              "Group_1","Group_2","PostHoc_p.val")) %>% as.list
  coloring_xlsx_cells(data.df = MeanDiff.df,
                      colors.list = colors.list,
                      which_cols.list = which_cols.list,
                      coloring_index.list = coloring_index.list,
                      save_path = path,
                      file_name = file_name) %>% suppressWarnings



  return(MeanDiff.df)
}
