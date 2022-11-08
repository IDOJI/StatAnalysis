StatResults_as.df = function(results.list, type="mean_diff", n_group=NULL, depth=2){
  if(depth==2){
    if(type=="mean_diff"){
      if(n_group==2){
        results.df = StatResults_as.df_2group_MeanDiff_2depth(results.list)
        return(results.df)
      }
    }
  }
}
