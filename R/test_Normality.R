test_Normality = function(X, group=NULL, which.col=NULL, alpha=0.05){
  ### as.list ===================================================================
  if(is.data.frame(X)){
    X.list = as_list_by(X, group)
    if(length(X.list)==2){
      NA.list = X.list[[2]]
      X.list = X.list[[1]]
    }
  }else{
    X.list = list(X)
    NA.list = NULL
  }
  ### NormTest by Length ========================================================
  norm_results = lapply(X.list, which.col, alpha, FUN=function(x, ...){
    # x = X.list[[1]]
    if(!is.null(which.col)){
      x = x[,which.col] %>% unlist
    }

    sample_size = length(x)
    if(sample_size<4){
      which_test = "Small Samplesize"
      results = "nonparametric"
      p.val = NA
    }else if(sample_size<5000){
      which_test = "Shapiro-Wilk"
      results = shapiro.test(x)
      p.val = results$p.value
    }else{
      which_test = "D'Agostino skewness"
      results = moments::agostino.test(x)
      p.val = results$p.value
    }
    is.normal = (p.val > alpha)==T
    results.df = data.frame(SampleSize=length(x), Norm_TestType=which_test, Norm_p.val = p.val, is.normal=is.normal)
    return(results.df)
  })
  ### Combine Results =======================================================
  NormTestResults.df = do.call(rbind, norm_results)
  NormTestResults.df = data.frame(Groups=rownames(NormTestResults.df), NormTestResults.df);rownames(NormTestResults.df)=NULL
  NormTestResults.df = cbind(c(group, rep(NA, nrow(NormTestResults.df)-1)), NormTestResults.df)
  names(NormTestResults.df)[1] = "Group Name"
  if(sum(is.na(NormTestResults.df$is.normal))==0){
    is.normal = sum(NormTestResults.df$is.normal)==nrow(NormTestResults.df)
  }else{
    is.normal = F
  }
  is.balanced = NormTestResults.df$SampleSize %>% unique %>% length == 1

  return(list(NormTestResults.df, is.normal=is.normal, is.balanced=is.balanced, NA.list))

}


# KS, SW, Lilliefors(LF), Anderson-Darling(AD) : aren't effective sample size less than 30
# 1)
# Non-parametric
# 2)
# Shapiro-Wilks : sample size < 2000 (not cover the very largest sample sizes)
# Kolmogorove-Smirnov : sample size < 2000
# The KS test is substantially less powerful for testing normality than other tests
# the Power of Shapio-Wilks test is low for small sample size
# 3)
# For large data samples, the D’Agostino Pearson test has shown to be the appropriate test as it would provide sensitivity for all conditions.
# see paper : Empirical Power Comparison Of Goodness of Fit Tests for Normality In The Presence of Outliers




