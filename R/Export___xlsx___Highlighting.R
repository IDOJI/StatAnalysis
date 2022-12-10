Export___xlsx___Highlighting = function(data.df,
                               colors.list,
                               which_cols.list,
                                         coloring_index.list,
                                         cols.list=NULL,
                                         rows.list=NULL,
                                         save_path,
                                         file_name){
  # colors.list = "red"
  # which_cols.list = 컬럼 위치
  # coloring_index.list : i번째 열에서 어떤 행에 coloring을 할 것인가
  # save_path = "C:/Users/IDO/OneDrive/github/Rpkgs/StatAnalysis"
  # file_name = "test"

  if(length(coloring_index.list)>0){


    ### Create a new workbook
    wb = createWorkbook()

    ### Add a worksheet
    addWorksheet(wb, sheetName = "results")

    ### write dataset
    writeData(wb, sheet="results", x=data.df, na.string = "")

    #===========================================================================
    # Highlighting
    #===========================================================================
    for(i in 1:length(which_cols.list)){
      #i=1
      ### define style
      style = createStyle(fgFill=colors.list[[i]])

      ### log2FoldChange
      y = which_cols.list[[i]]
      x = coloring_index.list[[i]]
      addStyle(wb, sheet="results", style=style, rows=x+1, cols=y, gridExpand=TRUE)
      # +1 for header line
    }


    ### Save Results
    save_path = path_tail_slash(save_path)
    saveWorkbook(wb, paste(save_path, file_name, ".xlsx", sep=""), overwrite=TRUE)
    cat("\n", crayon::blue("Writing an xlsx is done!"),"\n")
  }
}


