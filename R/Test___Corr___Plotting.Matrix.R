Test___Corr___Plotting.Matrix = function(data.df, cols=NULL, path, file.name){

  if(!is.null(cols)){
    data.df = data.df[,cols]
  }

  ### scatter plot
  png(paste(path, paste(paste("Correlation plot",file.name, sep="_"),"png", sep="."), sep="/"),width=2000, height=2000, res=300)
  plot(data.df)
  dev.off()

  ### correlation matrix
  cor.mat = cor(data.df)


  # # type1
  # png(paste(export.path, "Correlation plot_type_1.png", sep=""))
  # corrplot::corrplot(cor.mat, method="circle", type = "upper", tl.pos = "d")
  # corrplot::corrplot(cor.mat, add=TRUE, method="num", type = "lower",
  #                    diag = FALSE, tl.pos = "n", cl.pos = "n")
  # dev.off()
  # # type2
  # png(paste(export.path, "Correlation plot_type_2.png", sep=""))
  # corrplot::corrplot(cor.mat, method="circle")
  # dev.off()
  # # type3
  # png(paste(export.path, "Correlation plot_type_3.png", sep=""))
  # corrplot::corrplot(cor.mat, method="num")
  # dev.off()
  png(paste(path, paste(paste("Correlation plot",file.name, sep="_"),"png", sep="."), sep="/"),width=2000, height=2000, res=300)
  corrplot::corrplot(cor.mat,
                     method="shade", # 색 입힌 사각형
                     addshade="all", # 상관관계 방향선 제시
                     # shade.col=NA, # 상관관계 방향선 미제시
                     tl.col="red", # 라벨 색 지정
                     tl.srt=30, # 위쪽 라벨 회전 각도
                     diag=FALSE, # 대각선 값 미제시
                     addCoef.col="black", # 상관계수 숫자 색
                     order="FPC"
                     # "FPC": First Principle Component
                     # "hclust" : hierarchical clustering
                     # "AOE" : Angular Order of Eigenvectors
  )
  dev.off()
}
