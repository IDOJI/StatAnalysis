Test___MeanDiff___Multi.Reponses = function(##############################
                                            # data & variables
                                            ##############################
                                            df, var_group, var_responses,
                                            ##############################
                                            # Significance level
                                            ##############################
                                            alpha_Norm  = 0.05,
                                            alpha_Equal.Var  = 0.05,
                                            alpha_ANOVA = 0.05,
                                            alpha_PostHoc = 0.05,
                                            p.adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                                            ##############################
                                            # Figure
                                            ##############################
                                            # title="",
                                            # results.subtitle=T,
                                            ##############################
                                            # exporting
                                            ##############################
                                            save.path){
  #==================================================================================
  var_group_filename = gsub(pattern = "/", replacement = ".", x = var_group)
  var_responses_filename = gsub(pattern = "/", replacement = ".", x = var_responses)

  #==================================================================================
  MeanDiff_Multi_Responses.list = lapply(var_responses, FUN=function(y, ...){
    # y = var_responses[2]
    ind = which(var_responses==y)
    Test___MeanDiff___Single.Reponse(df,
                                     var_group,
                                     var_response = y,
                                     alpha_Norm, alpha_Equal.Var, alpha_ANOVA, alpha_PostHoc,
                                     p.adjust.method,
                                     save.path,
                                     filename = paste0("[ANOVA Results] ", '`', var_responses_filename[ind],"`"," by `", var_group_filename, "`"),
                                     export.xlsx = F)
  })


  #==================================================================================
  Combined_Final.list = lapply(MeanDiff_Multi_Responses.list, FUN=function(x){
    # x = MeanDiff_Multi_Responses.list[[1]]
    return(x[[1]])
  })
  Combined_Final.df = do.call(dplyr::bind_rows, Combined_Final.list) %>% as.data.frame




  #==================================================================================
  Combined_Reporting.list = lapply(MeanDiff_Multi_Responses.list, FUN=function(x){
    # x = MeanDiff_Multi_Responses.list[[1]]
    return(x[[2]])
  })
  Combined_Reporting.df = do.call(dplyr::bind_rows, Combined_Reporting.list) %>% as.data.frame



  #==================================================================================
  Test___MeanDiff___Exporting.XLSX.Highlight(Combined_Final.df,
                                             Combined_Reporting.df,
                                             alpha_ANOVA,
                                             alpha_PostHoc,
                                             save.path,
                                             filename = paste0("[ANOVA] Results_", "`", var_group_filename, "`"))

  return(list(Combined_Final.df, Combined_Reporting.df))
}

#
#
# Test___MeanDiff_Multi = function(#################################################
#                                  # data & variabels
#                                  #################################################
#                                  dataset.df,
#                                  variables,
#                                  group_variables,
#                                  #################################################
#                                  # significance level
#                                  #################################################
#                                  norm_alpha=0.05,
#                                  anova_alpha ,
#                                  posthoc_alpha,
#                                  p.adjust.method="bonferroni",
#                                  #################################################
#                                  # path & file names
#                                  #################################################
#                                  group_filenames,
#                                  path,
#                                  file_name = "MeanDiff"){
#
#
#   #==============================================================================
#   # Meandiff results for each group variable
#   #==============================================================================
#
#   MeanDiff_results.list = lapply(group_variables, FUN=function(ith_group_variable, ...){
#     ind = which(ith_group_variable==group_variables)
#
#     ith_filename = group_filenames[ind]
#
#     Final.list = lapply(variables, FUN=function(v, ...){
#       title = paste(v, "by", g,sep=" ")
#       filename = paste(v, "_", f, ".png", sep="")
#       Test___MeanDiff(#################################################
#                       # dataset
#                       #################################################
#                       X                 =    dataset.df,
#                       group             =    ith_group_variable,
#                       variable          =    v,
#                       #################################################
#                       # significance level
#                       #################################################
#                       norm_alpha        =    norm_alpha,
#                       anova_alpha       =    anova_alpha,
#                       posthoc_alpha     =    posthoc_alpha,
#                       p.adjust.method   =    p.adjust.method,
#                       #################################################
#                       # filename
#                       #################################################
#                       title             =    title,
#                       path              =    path,
#                       filename          =    filename)
#     })
#
#
#
#     return(Final_results.df)
#   })
#
#
#
#
#   #################################################
#   # combining results
#   #################################################
#   for(i in 1:length(Final.list)){
#     if(i==1){
#       Final_results.df = Final.list[[i]]
#     }else{
#       Final_results.df = rrbind(Final_results.df, Final.list[[i]])
#     }
#   }
#
#
#   ### combining
#   for(i in 1:length(MeanDiff_results.list)){
#     if(i==1){
#       MeanDiff.df = MeanDiff_results.list[[i]]
#     }else{
#       MeanDiff.df = rrbind(MeanDiff.df, MeanDiff_results.list[[i]])
#     }
#   }
#
#
#
#
#
#   #==============================================================================
#   # Highlighting results & Exporting
#   #==============================================================================
#   ### highlighting
#   which_meandiff_sig = which(MeanDiff.df$MeanDiff_p.val <= anova_alpha)
#   which_posthoc_sig = which(MeanDiff.df$PostHoc_p.val <= posthoc_alpha)
#   coloring_index.list = c(rep(list(which_meandiff_sig),3),
#                           rep(list(which_posthoc_sig),3))
#   colors.list = c(rep(list("#F4FA58"), 3),
#                   rep(list("#FE9A2E"), 3))
#   which_cols.list = which_cols(MeanDiff.df, c("Group Name","Response", "MeanDiff_p.val",
#                                               "Group_1","Group_2","PostHoc_p.val")) %>% as.list
#   coloring_xlsx_cells(data.df = MeanDiff.df,
#                       colors.list = colors.list,
#                       which_cols.list = which_cols.list,
#                       coloring_index.list = coloring_index.list,
#                       save_path = path,
#                       file_name = file_name) %>% suppressWarnings
#
#
#
#   return(MeanDiff.df)
# }

