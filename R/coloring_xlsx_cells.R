coloring_xlsx_cells = function(data.df,
                               colors.list,
                               columns.list,
                               coloring_index.list,
                               save_path,
                               file_name){
  # colors.list = "red"
  # columns.list = 컬럼 위치
  # coloring_index.list : i번째 열에서 어떤 행에 coloring을 할 것인가
  # save_path = "C:/Users/IDO/OneDrive/github/Rpkgs/StatAnalysis"
  # file_name = "test"

  if(length(coloring_index.list)>0){
    ### col.list
    if(length(colors.list)==1){
      colors.list = rep(x = colors.list %>% unlist,
                        times=length(coloring_index.list)) %>% as.list
    }


    ### write dataset
    wb = createWorkbook()
    addWorksheet(wb, sheetName="results")
    writeData(wb, sheet="results", x=data.df)

    for(i in 1:length(columns.list)){
      #i=1
      ### define style
      style <- createStyle(fgFill=colors.list[[i]])

      ### log2FoldChange
      y = columns.list[[i]]
      x = coloring_index.list[[i]]
      addStyle(wb, sheet="results", style=style, rows=x+1, cols=y, gridExpand=TRUE)
      # +1 for header line
    }

    # write result
    save_path = path_tail_slash(save_path)
    saveWorkbook(wb, paste(save_path, file_name, ".xlsx", sep=""), overwrite=TRUE)
    cat("\n", crayon::blue("Wrting a xlsx is done!"),"\n")
  }
}
