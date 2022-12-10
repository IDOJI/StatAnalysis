SUB___p.vals_signif_stars = function(p.vals){
  p.vals.signif = sapply(p.vals, FUN=function(p){
    if(p <= 0.0001){
      return("****")
    }else if(p <= 0.001){
      return("***")
    }else if(p <= 0.01){
      return("**")
    }else if(p <= 0.05){
      return("*")
    }else{
      return("NS")
    }
  })
  return(p.vals.signif)
}
