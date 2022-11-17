as_list_by = function(data.df, which.ID.col, arrange.by=NULL, is.date=F, desc=T, messaging=T){
  # data.df = data.df_new
  # which.ID.col = group

  # as_list_by(subjects.list[[1]], "Image_ID")
  # which.ID.col = "Gender"
  ### argument : arrange.by
  if(!is.null(arrange.by)){
    col.names = names(data.df)
    if(!arrange.by %in% col.names){
      stop("Check the colname in the argument 'arrange.by'")
    }
  }
  ### Extract ID
  ID = data.df[,which.ID.col] %>% unlist %>% as.factor

  if(class(ID)=="list"){
    ID = unlist(ID)
  }
  if(!is.null(dim(ID))){
    stop("Selected ID columns are more than one.")
  }else{
    ID_unique = unique(ID)
  }
  ### lapply
  data.list = lapply(ID_unique, data.df, which.ID.col, arrange.by, is.date, desc, FUN=function(x, data.df=data.df, which.ID.col=which.ID.col, arrange.by=arrange.by, is.date=is.date, desc=desc){
    ### x = ID_unique[1]
    ID_col = data.df[,which.ID.col] %>% unlist
    selected = data.df[ID_col == x,]
    text = paste("Listing each element : ", round(100 * which(ID_unique==x)/length(ID_unique), 4), "% is done", sep="")

    ### arrange by date
    if(!is.null(arrange.by)){
      x.col = selected[,arrange.by]
      # 변수가 date 값인 경우
      if(is.date){
        cat("\n", crayon::green(text), "\n")
        return(selected[order(x.col, decreasing=desc),])
        # 변수가 date 값이 아닌 경우
      }else{
        if(desc){
          cat("\n", crayon::green(text), "\n")
          dplyr::arrange(selected, desc(arrange.by)) %>% return
        }else{
          cat("\n", crayon::green(text), "\n")
          dplyr::arrange(selected, arrange.by) %>% return
        }
      }
    }else{
      if(messaging){
        cat("\n", crayon::blue(text), "\n")
      }
      return(selected)
    }
  })
  names(data.list) = ID_unique
  return(data.list)
}
