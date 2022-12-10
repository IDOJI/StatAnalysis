SUB___as.formula = function(y, x){
  if(length(y)>1){
    stop("'y' should have one element.")
  }

  x_form = paste(paste("`", x, "`", sep=""), collapse="+")
  y_form = paste("`", y, "`", sep="")

  return(as.formula(paste(y_form, x_form, sep="~")))
}
