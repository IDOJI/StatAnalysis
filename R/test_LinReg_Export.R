test_Normality = function(x, round.digits=10){
  # x = x.list[[n]]
  sample_size = length(x)

  # KS, SW, Lilliefors(LF), Anderson-Darling(AD) : aren't effective sample size less than 30
  # Non-parametric
  if(sample_size<4){
    text1 = crayon::blue("The sample size is less than 10.")
    text2 = crayon::blue("Use")
    text3 = crayon::bgMagenta("Non-parametric")
    text4 = crayon::blue("approach.")
    cat("\n", text1, "\n",text2, text3, text4, "\n")
    which_test = "CANNOT(Small n)"
    return(0)
    # Shapiro-Wilks : sample size < 2000 (not cover the very largest sample sizes)
    # Kolmogorove-Smirnov : sample size < 2000
    # The KS test is substantially less powerful for testing normality than other tests
    # the Power of Shapio-Wilks test is low for small sample size
  }else if(sample_size<5000){
    results = shapiro.test(x)
    which_test = "Shapiro-Wilk"
  }else{
    results = moments::agostino.test(x)
    which_test = "D'Agostino skewness"
    # For large data samples, the D’Agostino Pearson test has shown to be the appropriate test as it would provide sensitivity for all conditions.
    # see paper : Empirical Power Comparison Of Goodness of Fit Tests for Normality In The Presence of Outliers
  }
  p.val = results$p.value %>% format(,scientific=F) %>% as.numeric %>% round(digits = round.digits)
  results.df = data.frame(sample_size, which_test, p.val)
  names(results.df) = c("Normaliy_Sample.Size", "Normality_TestType", "Normality_p.val")
  return(results.df)
}
