test_MeanDiff_Multi_Export = function(data.df, group, variables, alpha=0.05, round.digits=100,
                                      colors.list = "#F781F3",
                                      save_path = "G:/내 드라이브/[백업] 공부/[백업] 공부_논문/논문_Ohio State Interventional Radiology",
                                      file_name = "[Results] Gender_MeanDiff"){
  # variables = c("PSY SUM", "PSYDEV",
  #               "EMOT SUM", "EMOTDEV",
  #               "SOC SUM", "SOCDEV",
  #               "PHYS SUM", "PHYSDEV",
  #               "SPIR SUM", "SPIRDEV",
  #               "INT SUM", "INTDEV",
  #               "XBAR", "VARIANCE",
  #               "WELLNESS",
  #               "BALANCE",
  #               "MAGNITUDE")

  ### multiple MeanDiff
  multi_results.list = list()
  for(m in 1:length(variables)){
    # m =1
    multi_results.list[[m]] = test_MeanDiff(data.df, group, variables[m], alpha, round.digits)
  }

  ### Adding a column
  multi_results_2.list = list()
  for(n in 1:length(multi_results.list)){
    nth.df = multi_results.list[[n]]
    multi_results_2.list[[n]] = cbind(Response=c(variables[n], rep(" ", nrow(nth.df)-1)), nth.df)
  }

  ### combining
  multi_results.df = do.call(rbind, multi_results_2.list)

  ### finding pval to color
  coloring_index.list = list()
  columns.list = which_col(multi_results.df, which.col = "_p.val") %>% as.list
  columns.list = c(1,columns.list)


  ### select rows for coloring except for NA
  coloring_index.list = list(NA)
  for(k in 2:length(columns.list)){
    #k=3
    kth_col = multi_results.df[,columns.list[[k]]] %>% unlist

    ind_num = which(!kth_col %>% is.na)
    ind_na = which(kth_col %>% is.na)

    kth_col[ind_na] = 1
    kth_col = as.numeric(kth_col)

    coloring_index.list[[k]] = which(kth_col<=alpha)
  }
  coloring_index.list[[1]] = coloring_index.list[[length(columns.list)]]


  ### color list
  if(length(colors.list)==1){
    colors.list = c("red", rep(colors.list,length(columns.list)))
  }else if(length(colors.list)==(length(coloring_index.list)-1)){
    colors.list = c("red", colors.list)
  }


  ###  coloring the index
  coloring_xlsx_cells(data.df = multi_results.df,
                      colors.list = colors.list,
                      columns.list = columns.list,
                      coloring_index.list = coloring_index.list,
                      save_path = save_path,
                      file_name = file_name)
}
