test_MeanDiff_Export_coloring = function(MeanDiff_results.df, alpha=0.05, colors.list = "#F781F3", save_path, file_name){
  # MeanDiff_results.df = gth_MeanDiff[[2]]
  # file_name = paste(excel_file_name, "PostHoc", sep="_"),
  # save_path = save_path
  # alpha=alpha
  # colors.list=colors.list
  ### find cols
  is_this_PostHoc = have_this(names(MeanDiff_results.df), "PostHoc_Groups") %>% length > 0
  if(is_this_PostHoc){
    selected_col = which_col(MeanDiff_results.df, "PostHoc_Groups", exact = F, as.col.names = F)
  }else{
    selected_col = which_col(MeanDiff_results.df, "Response", exact=T, as.col.names = F)
  }


  ### finding pval to color
  which_cols.list = which_col(MeanDiff_results.df, which.col = "p.val") %>% as.list
  which_cols.list = c(selected_col, which_cols.list)

  ### select rows for coloring except for NA
  coloring_index.list = list()
  for(k in 2:length(which_cols.list)){
    #k=2
    kth_col = MeanDiff_results.df[,which_cols.list[[k]]] %>% unlist

    ind_num = which(!kth_col %>% is.na)
    ind_na = which(kth_col %>% is.na)

    kth_col[ind_na] = 1
    kth_col = as.numeric(kth_col)

    coloring_index.list[[k]] = which(kth_col<=alpha)
  }# for
  coloring_index.list[[1]] = coloring_index.list[[length(which_cols.list)]]


  ### color list
  if(length(colors.list)==1){
    colors.list = c("red", rep(colors.list,length(which_cols.list)-1))
  }else if(length(colors.list)==(length(coloring_index.list)-1)){
    colors.list = c("red", colors.list)
  }

  ###  coloring the index
  coloring_xlsx_cells(MeanDiff_results.df,
                      colors.list = colors.list,
                      which_cols.list = which_cols.list,
                      coloring_index.list = coloring_index.list,
                      save_path = save_path,
                      file_name = file_name)
}
