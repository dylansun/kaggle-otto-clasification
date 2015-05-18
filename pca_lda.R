library(MASS)
data.train <- read.csv("train.csv")
data.test <- read.csv("test.csv")
data.train <- data.train[,-1]
data.test <- data.test[,-1]
train.fea <- data.train[,-94]
train.label <- data.train[,94]
data.fea.all <- rbind(train.fea , data.test)
pca <- prcomp(data.fea.all, scale = T)
summary(pca)

npr <- 40 
## obtain the principal component
pc <- pca$rotation[,1:npr]

## project the train data to the feature space
train.fea.projected <- as.matrix(train.fea) %*% as.matrix(pc)
data.train.projected <- data.frame(train.label, train.fea.projected)

## project the test data to the feature space
data.test.projected <- as.matrix(data.test) %*% as.matrix(pc)
data.test.projected <- data.frame(data.test.projected)

lda.fit = lda(train.label ~ . , data=data.train.projected)
## why this not return the prediction of the test??
## Caution use newdata rather than data- -
lda.pred = predict(lda.fit, newdata = data.test.projected)

# The function is currently defined as
#class.ind <- function(cl)
#{
#  n <- length(cl)
#  cl <- as.factor(cl)
#  x <- matrix(0, n, length(levels(cl)) )
#  x[(1:n) + n*(unclass(cl)-1)] <- 1
#  dimnames(x) <- list(names(cl), levels(cl))
#  x
#}
#m <- class.ind(lda.pred$class)
#lda.pred$posterior
result <- data.frame(id = 1:dim(m)[1], lda.pred$posterior)
write.csv(result, file = "pca_lda.csv",quote= FALSE, row.names = FALSE)
## 1.00381 
