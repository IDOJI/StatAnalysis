SUB___Have.Outlier = function(x, by="IQR"){
  if(by=="IQR"){
    upper_Q = fivenum(x)[4]
    lower_Q = fivenum(x)[2]
    IQR = upper_Q - lower_Q

    upper_outlier = x[which(x > upper_Q + 1.5*IQR)]
    lower_outlier = x[which(x < lower_Q - 1.5*IQR)]

    if(length(upper_outlier) > 0 && length(lower_outlier) > 0){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
}
