require(MASS)
pcaplot2d <- function(df, label){
  
  df <- data.matrix(df, rownames.force = F)
  pca <- prcomp(df, scale = T)
  pr <- pca$rotation[,1:2]
  d  <- df %*% pr
  plot(x = d[,1], y = d[,2], col = as.factor(label) )
}

pcaplot3d <- function(df, label){
  
  df <- data.matrix(df, rownames.force = F)
  pca <- prcomp(df, scale = T)
  pr <- pca$rotation[,1:3]
  d  <- df %*% pr
  plot(x = d[,1], y = d[,2],  z = d[,3],col = as.factor(label) )
}
