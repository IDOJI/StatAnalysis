Test___Normality = function(x.vec, alpha=0.05){
  n = length(x.vec)
  results.list = list(NA, NA, NA, NA)
  names(results.list) = c("Results", "What.Test", "p-val", "is.Normal")

  if(n<4){
    results.list[[1]] = "Small Sample"
    results.list[[2]] = "Nonparametric needed"
    results.list[[3]] = 0
  }else if(n < 5000){
    results.list[[1]] = shapiro.test(x.vec)
    results.list[[2]] = "Shapiro-Wilk"
    results.list[[3]] = results.list[[1]]$p.value
  }else{
    results.list[[1]] = moments::agostino.test(x.vec)
    results.list[[2]] = "D'Agostino skewness"
    results.list[[3]] = results.list[[1]]$p.value
  }

  results.list[[4]] = results.list[[3]] > 0.05
  return(results.list)
}
















#
#
# '
# # KS, SW, Lilliefors(LF), Anderson-Darling(AD) : aren't effective sample size less than 30
# # 1)
# # Non-parametric
# # 2)
# # Shapiro-Wilks : sample size < 2000 (not cover the very largest sample sizes)
# # Kolmogorove-Smirnov : sample size < 2000
# # The KS test is substantially less powerful for testing normality than other tests
# # the Power of Shapio-Wilks test is low for small sample size
# # 3)
# # For large data samples, the D’Agostino Pearson test has shown to be the appropriate test as it would provide sensitivity for all conditions.
# # see paper : Empirical Power Comparison Of Goodness of Fit Tests for Normality In The Presence of Outliers
#



