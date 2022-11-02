stat_results_as_df = function(results.list, type="mean_diff", n_group=NULL, depth=2){
  if(depth==2){
    if(type=="mean_diff"){
      if(n_group==2){
        results.df = stat_results_as_df_MeanDiff_2group_2depth(results.list)
        return(results.df)
      }
    }
  }
}
