test_MeanDiff_Multi = function(data.df, group, variables, exclude_rows=NULL, ex_reasons=NULL, alpha=0.05, alpha_adj=c("None", "Bonferroni"), PostHoc=c("Significant", "All"), round.digits=100, Chronbach=NULL){
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
    # x= variables[1]
    return(test_MeanDiff(data.df, group, variable=x, exclude_rows, ex_reasons, round.digits, alpha))
  })



  #===========================================================================
  # Finding Significant Cases
  #===========================================================================
  Which_significant = sapply(MeanDiff_results.list, alpha_adjusted, FUN=function(x, ...){
    # x = MeanDiff_results.list[[2]]
    if(x$MeanDiff_p.val[1] > alpha_adjusted){
      return(F)
    }else{
      return(T)
    }
  })
  if(alpha_adj=="Bonferroni"){
    alpha_adjusted_posthoc = alpha/sum(Which_significant) # 실험별 오류율
  }else if(alpha_adj=="None"){
    alpha_adjusted = alpha
  }
  MeanDiff_results.list = c(MeanDiff_results.list[Which_significant], MeanDiff_results.list[!Which_significant])



  #===========================================================================
  # Tables for exporting results
  #===========================================================================
  Table = matrix("", 20, 3) %>% as.data.frame
  # group
  Group = MeanDiff_results.list[[1]][,4]
  Group = c(group, Group)
  # sample size
  Sample.Size = MeanDiff_results.list[[1]][,5] %>% as.numeric
  Sample.Size = c(paste("Total", sum(Sample.Size), sep = " : "), Sample.Size)
  # sig levels
  MeanDiff_Sig.Level = c("MeanDiff_Sig.Level", alpha_adjusted, paste("(", alpha_adj, " corrected)", sep=""))
  PostHoc_Sig.Level = c("PostHoc_Sig.Level", alpha_adjusted_posthoc, paste("(", alpha_adj, " corrected)", sep=""))
  Sig.Levels = c(MeanDiff_Sig.Level, PostHoc_Sig.Level)
  # inserting
  Table[1:length(Group),1] = Group
  Table[1:length(Sample.Size),2] = Sample.Size
  Table[1:length(Sig.Levels),3] = Sig.Levels
  which_rows = apply(Table,MARGIN=1, FUN=function(x){
    return(sum(x==""))
  })
  Table = Table[1:(which(which_rows==3)[2]),]
  names(Table) = c("Group(Trt)", "Sample.Size", "Sig.Levels")


  #===========================================================================
  # Chronbach
  #===========================================================================
  if(!is.null(Chronbach)){
    col_names = names(Chronbach.df)
    names(Chronbach.df) = names(Table)
    Table = rbind(Table, rbind(col_names, Chronbach.df))
  }



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
    # x = PostHoc_results.list[[1]]
    if(is.null(x[[2]])){
      y = x[[1]]
      z = NULL
    }else{
      y = x[[1]]
      z = x[[2]]
    }

    MeanDiff_results.df = y[1,c(1,10,11)]


    if(is.null(z)){
      PostHoc_results.df = matrix("", 1, 4) %>% as.data.frame
      names(PostHoc_results.df) = c("PostHoc_TestType", "Overall_Difference","PostHoc_Groups", "PostHoc_p.val")
      PostHoc_results.df[1,2] = "No"
    }else{
      # # p.val
      # p.val_PostHoc = z$p.val %>% as.numeric
      # which_significant_PostHoc = which(p.val_PostHoc<=alpha_adjusted_posthoc)
      # PostHoc_p.val = p.val_PostHoc[which_significant_PostHoc]
      # # Testtype
      # PostHoc_TestType = z$PostHoc_TestType[1:length(which_significant_PostHoc)]
      # # significant post hoc group
      # PostHoc_Groups = z$PostHoc_Groups[which_significant_PostHoc]
      # # reulst data.frame
      PostHoc_results.df = data.frame(PostHoc_TestType=z$PostHoc_TestType,
                                      Overall_Difference=c("Yes",rep("", length(z$PostHoc_TestType)-1)),
                                      PostHoc_Groups=z$PostHoc_Groups,
                                      PostHoc_p.val=z$p.val)


      MeanDiff_results.df_dummy = matrix("", nrow(PostHoc_results.df)-nrow(MeanDiff_results.df), ncol(MeanDiff_results.df)) %>% as.data.frame
      names(MeanDiff_results.df_dummy) = names(MeanDiff_results.df)
      MeanDiff_results.df = rbind(MeanDiff_results.df, MeanDiff_results.df_dummy)
    }
    PostHoc_results.df = change_colnames(PostHoc_results.df, "Overall_Difference", "Overall Difference")
    PostHoc_results.df = change_colnames(PostHoc_results.df, "PostHoc_Groups", "Which pairs showed difference(Highlighted)")


    final.df = cbind(MeanDiff_results.df, PostHoc_results.df)
    final.df = change_class(final.df, which.col = "PostHoc_p.val", what.class = "numeric")
    return(final.df)
  })


  #===========================================================================
  # nrows for merging
  #===========================================================================
  nrows_for_merging.list = sapply(Final_results.list, simplify=T, FUN=function(x){
    # x=Final_results.list[[5]]
    if(nrow(x)!=1){
      return(nrow(x))
    }
  })
  nrows_for_merging_2.list = list()
  for(i in 1:length(nrows_for_merging.list)){
    #i=2
    k = (i-1)*10
    ith = nrows_for_merging.list[[i]]
    if(!is.null(ith)){
      nrows_for_merging_2.list[[i]] = (1+k):(ith+k)
    }
  }




  #===========================================================================
  # combining
  #===========================================================================
  Final_results.df = do.call(rbind, Final_results.list)
  rownames(Final_results.df) = NULL

  ## adding "Table"
  if(nrow(Table)<nrow(Final_results.df)){
    n_row = nrow(Final_results.df)-nrow(Table)
    empty.df = matrix("", n_row, ncol(Table)) %>% as.data.frame()
    names(empty.df) = names(Table)
    Table = rbind(Table, empty.df)
  }else{
    n_row = nrow(Table)-nrow(Final_results.df)
    empty.df = matrix("", n_row, ncol(Final_results.df)) %>% as.data.frame()
    names(empty.df) = names(Final_results.df)
    Final_results.df = rbind(Final_results.df, empty.df)
  }

  Final_results_2.df = cbind(Table, Final_results.df)
  Final_results_2.df$PostHoc_p.val = as.numeric(Final_results_2.df$PostHoc_p.val)
  Final_results_2.df$MeanDiff_p.val = as.numeric(Final_results_2.df$MeanDiff_p.val)
  return(list(Final_results_2.df, nrows_for_merging_2.list))
}
