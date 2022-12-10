Summary___Demo = function(data.df, num.demo.col=NULL, cha.demo.col=NULL, group.col=NULL, group.as.row=F, path=NULL, file.name=NULL){
  # num.demo.col = c("Age", "AHI")
  # cha.demo.col = "Sex"
  # group.col = "AHI_groups"
  # data.df = dataset.df
  if(!is.null(num.demo.col)){
    num.demo_results = summary_mean_pm_sd_groups(data.df, demo.col = num.demo.col, group.col = group.col, group.as.row = group.as.row)
  }else{
    num.demo_results = NULL
  }
  if(!is.null(cha.demo.col)){
    cha.demo_results = summary_prop_groups(data.df, demo.col=cha.demo.col, group.col=group.col, group.as.row=group.as.row)
  }else{
    cha.demo_results = NULL
  }
  if(!is.null(num.demo.col)||!is.null(char.demo.col)){
    if(group.as.row){
      summary_results=cbind(num.demo_results, cha.demo_results)
    }else{
      summary_results=rbind(num.demo_results, cha.demo_results)
    }
  }
  if(!is.null(path) && !is.null(file.name)){
    dir.create(path, showWarnings = F)
    write.xlsx(summary_results, file = paste(path, file.name, sep="/"), overwrite = T, rowNames=T, colNames=T)
  }
  return(summary_results)
}
