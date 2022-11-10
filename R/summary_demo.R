summary_demo = function(data.df, num.demo.col, char.demo.col, group.col=NULL, group.as.row=F){
  # num.demo.col = c("Age", "AHI")
  # char.demo.col = "Sex"
  # group.col = "AHI_groups"
  num.demo_results = summary_mean_pm_sd_groups(data.df, demo.col = num.demo.col, group.col = group.col, group.as.row = group.as.row)
  char.demo_results = summary_prop_groups(data.df, demo.col=char.demo.col, group.col=group.col, group.as.row=group.as.row)
  if(group.as.row){
    return(cbind(num.demo_results, char.demo_results))
  }else{
    return(rbind(num.demo_results, char.demo_results))
  }
}
