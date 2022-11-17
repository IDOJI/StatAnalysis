test_Chronbach = function(data.df, questions_section_unit=6, section.names, path=NULL){
  # questions_section_unit=6 : 1~6까지 한 묶음, 7~12까지 한 묶음으로 간주

  if(ncol(data.df)%%questions_section_unit!=0){
    stop("ncol is not divisible by the unit.")
  }

  chronbach_results = list()
  for(i in 1:(ncol(data.df)/questions_section_unit)){
    k = (i-1)*6
    chronbach_results[[i]] = psy::cronbach(data.df[,(1+k):(i*6)])
    names(chronbach_results)[i] = section.names[i]
  }
  col_names = names(chronbach_results)
  chronbach_results.df = do.call(rbind, chronbach_results)
  if(!is.null(path)){
    write.csv(chronbach_results.df, file = paste(path, "Chronbach Alphas.csv", sep="/"))
  }
  return(chronbach_results.df)
}
