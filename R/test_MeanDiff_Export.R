test_MeanDiff_Export = function(data.df, gth_MeanDiff, alpha=0.05,
                                colors.list = "#F781F3",
                                save_path = "G:/내 드라이브/[백업] 공부/[백업] 공부_논문/논문_Ohio State Interventional Radiology",
                                excel_file_name){

  if(length(gth_MeanDiff)==2){
    # exporting colored excel file
    MD = gth_MeanDiff[[1]]
    MC = gth_MeanDiff[[2]]
    test_MeanDiff_Export_coloring(MD, file_name = excel_file_name, save_path = save_path, alpha=alpha, colors.list=colors.list)
    if(!is.list(MC) && is.data.frame(MC)){
      MC.list = list(MC)
    }
    for(k in 1:length(MC)){
      # k=1
      kth_MC = MC[[k]]
      response = kth_MC$Response[1]
      kth_excel_file_name = paste(excel_file_name, "_PostHoc","_(",response,")", sep="")
      kth_plot_file_name = paste(kth_excel_file_name, "Boxplot", sep="_")
      test_MeanDiff_Export_coloring(kth_MC, file_name = kth_excel_file_name, save_path = save_path, alpha=alpha, colors.list=colors.list)

      # exporting boxplot
      test_MeanDiff_Export_boxplot(data.df = data.df,
                                   MeanDiff_results.df = kth_MC,
                                   alpha = alpha,
                                   save.path = save_path,
                                   filename = kth_plot_file_name)
    }
  }else{
    test_MeanDiff_Export_coloring(gth_MeanDiff, file_name = file_name, save_path = save_path, alpha=alpha, colors.list=colors.list)
  }
}
################################################################################
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
###############################################################################
test_MeanDiff_Export_boxplot = function(data.df, MeanDiff_results.df, alpha, save.path, filename){
  # 아직은 ANOVA의 경우만 가능
  # MeanDiff_results.df = ith_MC.list[[1]]
  # Gender
  # MeanDiff_results.df = MeanDiff.list[[1]]
  # data.df
  ### finding significant p.val
  index.list = list()
  selected_col = which_col(MeanDiff_results.df, which.col = "p.val")
  if(length(selected_col)!=1){
    selected_col = which_col(MeanDiff_results.df, which.col = "MeanDiff_p.val")
  }else{
    col = MeanDiff_results.df[,selected_col] %>% unlist %>% as.numeric %>% suppressWarnings ;names(col)=NULL

    ind_num = which(!col %>% is.na)
    ind_na = which(col %>% is.na)

    col[ind_na] = 1
    coloring_index = which(col<=alpha)

    if(length(coloring_index)>0){
      qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
      col.vec = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

      main = paste("Boxplot of", "'",MeanDiff_results.df$Response[1], "'","for", MeanDiff_results.df$`Groups(Trt)` [1])

      response = data.df[,MeanDiff_results.df$Response[1]] %>% unlist
      group = data.df[,MeanDiff_results.df$`Groups(Trt)`[1]] %>% unlist


      ### exporting png
      png(filename = paste(save.path,paste(filename,"png",sep="."),sep="/"))
      bp = boxplot(response~group,
                   ylab = MeanDiff_results.df$Response[1],
                   #xlab=MeanDiff_results.df$`Groups(Trt)`[1],
                   main=main,
                   col=sample(col.vec, length(unique(group))),
                   xaxt="n")
      tick = seq_along(bp$names)
      axis(1, at = tick, labels = FALSE)
      text(tick, par("usr")[3] - 2, bp$names, srt = 45, xpd = TRUE)
      dev.off()

      cat("\n",crayon::blue("Exporting a boxplot is done!"),"\n")
    }# if length(coloring_index)>0
  }# else
}

