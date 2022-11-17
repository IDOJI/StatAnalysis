test_MeanDiff_Multi = function(data.df, group, variables, exclude_rows=NULL, ex_reasons=NULL, alpha=0.05, alpha_adj=c("None", "Bonferroni"), PostHoc=c("Significant", "All"), round.digits=100){
  #===========================================================================
  # Adjust Alpha
  #===========================================================================
  if(alpha_adj=="Bonferroni"){
    alpha_adjusted = alpha/length(variables) # 실험별 오류율
  }else if(alpha_adj=="None"){
    alpha_adjusted = alpha
  }


  #===========================================================================
  # MeanDiff Test
  #===========================================================================
  MeanDiff_results.list = lapply(variables, data.df, group, exclude_rows, ex_reasons, round.digits, alpha, FUN=function(x, ...){
    return(test_MeanDiff(data.df, group, variable=x, exclude_rows, ex_reasons, round.digits, alpha))
  })



  #===========================================================================
  # Finding Significant Cases
  #===========================================================================
  Which_significant = sapply(MeanDiff_results.list, alpha_adjusted, FUN=function(x, ...){
    # x = MeanDiff_results.list[[1]]
    if(x$MeanDiff_p.val[1] > alpha_adjusted){
      return(F)
    }else{
      return(T)
    }
  })
  alpha_adjusted_posthoc = alpha/sum(Which_significant)



  #===========================================================================
  # Post-Hoc : Multiple Comparison
  #===========================================================================
  PostHoc_results.list = lapply(MeanDiff_results.list, data.df, PostHoc, round.digits, alpha_adjusted, alpha_adjusted_posthoc, FUN=function(x, ...){
    # x=MeanDiff_results.list[[5]]


    # 평균차이의 significancy에 관계없이 다중 검정을 적용 (실험별 오류율)
    if(PostHoc == "All"){
      posthoc_results = test_MeanDiff_PostHoc(MeanDiff_results.df=x, data.df, alpha=alpha_adjusted, round.digits)
    # 평균차이의 significant인 경우만 다중 검정을 적용 (실험별 오류율)
    }else if(PostHoc == "Significant"){
      if(x$MeanDiff_p.val[1] > alpha_adjusted){
        posthoc_results = NULL
      }else{
        posthoc_results = test_MeanDiff_PostHoc(MeanDiff_results.df=x, data.df, alpha=alpha_adjusted_posthoc, round.digits)
      }
    }else{
      posthoc_results = NULL
    }
    return(list(x, posthoc_results))
  })



  #===========================================================================
  # Final results
  #===========================================================================
  Final_results.list = lapply(PostHoc_results.list, alpha, alpha_adjusted, alpha_adjusted_posthoc, PostHoc, FUN=function(x, ...){
    # x = PostHoc_results.list[[6]]
    if(is.null(x[[2]])){
      y = x[[1]]
      z = NULL
    }else{
      y = x[[1]]
      z = x[[2]]
    }

    MeanDiff_results.df = y[,c(1,4,5,10,11)]


    if(is.null(z)){
      MeanDiff_results.df = cbind(MeanDiff_results.df, MeanDiff_Alpha=c(alpha_adjusted, rep(NA, nrow(MeanDiff_results.df)-1)))
      PostHoc_results.df = matrix("", nrow(MeanDiff_results.df), 4) %>% as.data.frame
    }else{
      if(PostHoc=="All"){
        what.alpha = c(alpha_adjusted,rep("", nrow(z)-1))
      }else{
        what.alpha = c(alpha_adjusted_posthoc,rep("", nrow(z)-1))
      }
      PostHoc_results.df = data.frame(z$PostHoc_TestType, z$PostHoc_Groups, z$p.val, what.alpha)

      MeanDiff_results.df_dummy = matrix("", nrow(z)-nrow(MeanDiff_results.df), ncol(MeanDiff_results.df)) %>% as.data.frame
      names(MeanDiff_results.df_dummy) = names(MeanDiff_results.df)
      MeanDiff_results.df_dummy = change_class(MeanDiff_results.df_dummy, which.col = c("Normaliy_Sample.Size", "MeanDiff_p.val"), what.class = "numeric")
      MeanDiff_results.df = rbind(MeanDiff_results.df, MeanDiff_results.df_dummy)
      MeanDiff_results.df = cbind(MeanDiff_results.df, MeanDiff_Alpha=c(alpha_adjusted, rep(NA, nrow(MeanDiff_results.df)-1)))
    }
    names(PostHoc_results.df) = c("PostHoc_TestType", "PostHoc_Groups","PostHoc_p.val", "PostHoc_Alpha")
    final.df = cbind(MeanDiff_results.df, PostHoc_results.df)
    final.df = change_class(final.df, which.col = "PostHoc_p.val", what.class = "numeric")
    return(final.df)
  })
  Final_results.df = do.call(rbind, Final_results.list)
  rownames(Final_results.df) = NULL
  Final_results.df = change_colnames(Final_results.df, from = "Normaliy_Sample.Size", to = "Sample.Size")
  return(list(Final_resulst=Final_results.df, alpha=alpha, alpha_adjusted=alpha_adjusted, alpha_adjusted_posthoc=alpha_adjusted_posthoc))
}








