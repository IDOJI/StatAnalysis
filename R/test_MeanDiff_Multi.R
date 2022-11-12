test_MeanDiff_Multi = function(data.df, group, variables, exclude_rows=NULL, ex_reasons=NULL, alpha=0.05, alpha_PostHoc=0.05, round.digits=100){
  ### multiple MeanDiff
  multi_results.list = list()
  for(m in 1:length(variables)){
    # m=1
    multi_results.list[[m]] = test_MeanDiff(data.df,
                                            group,
                                            variable = variables[m],
                                            exclude_rows = exclude_rows,
                                            ex_reasons=ex_reasons,
                                            alpha,
                                            alpha_PostHoc,
                                            round.digits)
  }


  ### dividing
  MeanDiff.list = list()
  MultComp.list = list()
  for(i in 1:length(multi_results.list)){
    ith = multi_results.list[[i]]
    if(length(ith)==2 && !is.data.frame(ith)){
      MeanDiff.list[[i]] = ith[[1]]
      MultComp.list[[i]] = ith[[2]]
    }else{
      MeanDiff.list[[i]] = ith
    }
  }
  MeanDiff.list = refineR::rm_list_null(MeanDiff.list)
  MeanDiff_Combined.df = do.call(rbind, MeanDiff.list)

  if(length(MultComp.list)!=0){
    MultComp.list = refineR::rm_list_null(MultComp.list)
    return(list(Mean_Difference=MeanDiff_Combined.df, Multiple_Comparison=MultComp.list))
  }else{
    return(MeanDiff_Combined.df)
  }
}
