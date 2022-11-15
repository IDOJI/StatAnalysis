test_MeanDiff_Multi = function(data.df, group, variables, exclude_rows=NULL, ex_reasons=NULL, round.digits=100){

  ### multiple MeanDiff
  multi_results.list = list()
  for(m in 1:length(variables)){
    # m=1
    multi_results.list[[m]] = test_MeanDiff(data.df,
                                            group,
                                            variable = variables[m],
                                            exclude_rows = exclude_rows,
                                            ex_reasons=ex_reasons,
                                            round.digits)

    #=============================================================================
    # Post-Hoc : Multiple Comparison
    #=============================================================================
    if(results.df$MeanDiff_p.val[1]<=alpha){
      results.list = list()
      results.list[[1]] = results.df
      results.list[[2]] = test_MeanDiff_PostHoc(data.df, group, variable, alpha, alpha_adj, MeanDiff_results.df = results.df)
      return(results.list)
    print(m)
  }


  ### dividing
  MeanDiff.list = list()
  MultComp.list = list()
  Results_table.list = list()
  for(i in 1:length(multi_results.list)){
    # i=1
    ith = multi_results.list[[i]]
    if(length(ith)==3){
      MeanDiff.list[[i]] = ith[[1]]
      MultComp.list[[i]] = ith[[2]]
      Results_table.list[[i]] = ith[[3]]
    }else if(length(ith)==2){
      MeanDiff.list[[i]] = ith[[1]]
      Results_table.list[[i]] = ith[[2]]
    }
  }
  MeanDiff.list = refineR::rm_list_null(MeanDiff.list)
  MeanDiff_Combined.df = do.call(rbind, MeanDiff.list)
  Results_table.df = do.call(rbind, Results_table.list)

  if(length(MultComp.list)!=0){
    MultComp.list = refineR::rm_list_null(MultComp.list)
    return(list(Mean_Difference=MeanDiff_Combined.df, Multiple_Comparison=MultComp.list, Results_table=Results_table.df))
  }else{
    return(list(Mean_Difference=MeanDiff_Combined.df, Results_table=Results_table.df))
  }
}

