test_MeanDiff = function(data.df,
                         group,
                         variable,
                         exclude_rows=NULL,
                         ex_reasons=NULL,
                         round.digits=100,
                         alpha=0.05){
  # data.df = data
  # group = "Years"
  # group = "Ethnicity"
  # variable = "WELLNESS"
  # variable = x

  ### exclude some rows
  # ex_reasons = "Small Sample Size"
  if(!is.null(exclude_rows)){
    excluded = data.df[exclude_rows,group] %>% unlist
    data.df = data.df[-exclude_rows,]
  }


  ### Normality & Homoscedasticity
  results_NormHomo.df = test_Group_Normality_and_Homoscedasticity(data.df, group, variable, alpha=alpha)

  ### group vectors
  cont_var = data.df[,variable] %>% unlist
  group_var = data.df[,group] %>% unlist %>% as.factor
  n_group = group_var %>% unique %>% length


  ### p.val
  norm_p.val = exclude_na(results_NormHomo.df$Normality_p.val)
  homo_p.val = exclude_na(results_NormHomo.df$Homoscedasticity_p.val)

  ### criterion
  is.normal = sum(norm_p.val>alpha)==nrow(results_NormHomo.df)
  is.homo = (homo_p.val>alpha)

  ### MeanDiff
  if(n_group==1){
    stop("There is only one group.")
  }else if(n_group==2){
    results_MeanDiff = test_MeanDiff_2group(data.df, group, variable, is.normal, is.homo, round.digits)
  }else{
    results_MeanDiff = test_MeanDiff_3group(data.df, group, variable, is.normal, is.homo, round.digits)
  }

  ### excluding vec & response col
  response_col_1 = c(variable, rep(" ", n_group-1))
  if(is.null(exclude_rows)){
    excluded.vec = rep(" ", length(response_col_1))
  }else{
    if(is.null(ex_reasons)){
      bec = NULL
    }else{
      bec = "∵"
    }
    excluded.vec = c(c(exclude_rows, excluded, paste("(", bec, ex_reasons, ")", sep="")),
                     rep(" ", length(response_col_1)-3))
  }

  ### Adding response & groups
  final_results = cbind(Response=response_col_1,
                        Excluded_Group_Rows=excluded.vec,
                        Group=c(group, rep(" ", n_group-1)),
                        results_NormHomo.df,
                        results_MeanDiff) %>% as.data.frame
  return(final_results)
}



