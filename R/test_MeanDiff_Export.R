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

