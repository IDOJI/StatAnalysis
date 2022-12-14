Test___Corr___Exporting.XLSX.Highlight = function(Corr.Test_results.df,
                                                  alpha,
                                                  save.path,
                                                  filename){
  ### create directory
  dir.create(save.path, showWarnings = F)


  ### untibble
  results.df = as.data.frame(Corr.Test_results.df)


  ## which cols
  which_vars = c(1,2)
  which_pval = c(5,6,7)
  which_cols.list = c(which_vars, which_pval) %>% as.list


  ### what colors
  colors_vars = rep("#ABEBC6", length(which_vars)) %>% as.list
  colors_pval = rep("#F1948A", length(which_pval)) %>% as.list
  colors.list = c(colors_vars, colors_pval) %>% as.list


  ### which rows
  which_rows = which(results.df$p.value <= alpha)
  which_rows.list = rep(list(which_rows), length(which_cols.list))


  Export___xlsx___Highlighting(data.df             = results.df,
                               colors.list         = colors.list,
                               which_cols.list     = which_cols.list,
                               coloring_index.list = which_rows.list,
                               save.path           = save.path ,
                               file_name           = filename,
                               sheet.name          = "Corr Test Results")

  cat("\n", crayon::yellow(paste0("[", "Correlation Test","] ", "x", "  vs  ")), crayon::red(unique(results.df$y)), crayon::blue("is exported !"),"\n")
}














