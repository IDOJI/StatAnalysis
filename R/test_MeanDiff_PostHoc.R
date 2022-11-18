test_MeanDiff_PostHoc = function(MeanDiff_results.df, data.df, alpha=0.05, round.digits=100){
  # MeanDiff_results.df=x
  group = MeanDiff_results.df$Group[1]
  variable = MeanDiff_results.df$Response[1]

  group_var = data.df[,group] %>% unlist
  cont_var = data.df[,variable] %>% unlist

  test.type = MeanDiff_results.df$MeanDiff_TestType[1]
  PH.list = list()

  ##############################################################################
  ### Tukey, Scheffe, LSD
  ##############################################################################
  if(test.type=="One-Way ANOVA(Balanced)"){
    test_names = c("Tukey-Kramer", "Scheffe", "SNK")
    methods = c("hsd", "scheffe")
    for(i in 1:length(test_names)){
      if(test_names[i]=="SNK"){
        df = df.residual((results_MeanDiff))
        MSerror = deviance(results_MeanDiff)/df
        PH = SNK.test(cont_var, group_var, df, MSerror, alpha, group = F)
        PH.df = change_colnames(PH$comparison, "pvalue", "p.val", exact.from = T)
        PH.df = gather_col(data.df = PH.df, col.words = "p.val", exact = T, where = ncol(PH.df))
        PH.df = cbind(c(test_names[i], rep(" ", nrow(PH.df)-1)),rownames(PH.df), PH.df)
        rownames(PH.df)=NULL
      }else{
        PH = DescTools::PostHocTest(results_MeanDiff, method=methods[i], conf.level=1-alpha)
        PH.df = cbind(c(test_names[i], rep(" ", nrow(PH$group_var)-1)),rownames(PH$group_var), PH$group_var) %>% as.data.frame
        rownames(PH.df) = NULL
      }
      PH.list[[i]] = PH.df
    }
    PH.df = do.call(cbind, PH.list)
  ##############################################################################
  ### Tukey, Scheffe, LSD
  ##############################################################################
  }else if(test.type=="One-Way ANOVA(Imbalanced)"){
    test_names = c("Tukey-Kramer", "Scheffe", "LSD", "SNK")
    methods = c("hsd", "scheffe", "lsd")
    for(i in 1:length(test_names)){
      if(test_names[i]=="SNK"){
        df = df.residual((results_MeanDiff))
        MSerror = deviance(results_MeanDiff)/df
        PH = SNK.test(cont_var, group_var, df, MSerror, alpha, group = F)
        PH.df = change_colnames(PH$comparison, "pvalue", "p.val", exact.from = T)
        PH.df = gather_col(data.df = PH.df, col.words = "p.val", exact = T, where = ncol(PH.df))
        PH.df = cbind(c(test_names[i], rep(" ", nrow(PH.df)-1)),rownames(PH.df), PH.df)
        names(PH.df)[1:2] = c("PostHoc_TestType", "PostHoc_Groups")
        rownames(PH.df)=NULL
      }else{
        PH = DescTools::PostHocTest(results_MeanDiff, method=methods[i], conf.level=1-alpha)
        PH.df = cbind(c(test_names[i], rep(" ", nrow(PH$group_var)-1)),rownames(PH$group_var), PH$group_var) %>% as.data.frame
        rownames(PH.df) = NULL
      }
      PH.list[[i]] = PH.df
    }
    PH.df = do.call(cbind, PH.list)
  ##############################################################################
  ### Games-Howell
  ##############################################################################
  }else if(test.type=="Welch ANOVA(Balanced)" || test.type=="Welch ANOVA(Imbalanced)"){
    # Games-Howell, Dunnett T3, Dunnett C, Tamhane T2
    # Games-Howell :
    # no equal sample size needed, works even with
    # heteroscedastic data
    PH = test_MeanDiff_PostHoc_games.howell(group_var, cont_var, alpha)
    p.val = PH$p.val
    PH$p.val = NULL
    PH.df = cbind(c("Games-Howell", rep(" ", nrow(PH)-1)), PH, p.val=p.val) %>% as.data.frame

  ##############################################################################
  ### Dunn(Bonferroni)
  ##############################################################################
  }else if(test.type=="Kruskal-Wallis(Balanced)" || test.type=="Kruskal-Wallis(Imbalanced)"){
    PH = DescTools::DunnTest(x=cont_var, g=group_var, method="bonferroni")[[1]]
    PH.df = cbind(c("Dunn(Bonferroni)", rep(" ", nrow(PH)-1)),rownames(PH), PH) %>% as.data.frame
    PH.df = change_colnames(PH.df, "pval", "p.val");rownames(PH.df)=NULL
  }else{
    warnings("There is no condition of test.type for Post-Hoc.")
  }



  ##############################################################################
  ### Return
  ##############################################################################
  names(PH.df)[c(1:2,4)] = c("PostHoc_TestType", "PostHoc_Groups", "p.val")
  PH.df = cbind(c(group, rep(" ", nrow(PH.df)-1)), c(variable, rep(" ", nrow(PH.df)-1)),PH.df)
  names(PH.df)[1:2] = c("Groups(Trt)", "Response")
  return(PH.df)
}
