library(caret)
require(xgboost)
require(methods)
data = read.csv('data/train.csv',header=TRUE,stringsAsFactors = F)
index <- createDataPartition(data$target, list = F, p = .8)
data.train <- data[index,]
data.test <- data[-index,]
data.train <- data.train[,-1]
data.test <- data.test[,-1]

y = data.train[, ncol(data.train)]
y = gsub("Class_", '', y)
y = as.integer(y) - 1

test.y = data.test[, ncol(data.test)]
test.y = gsub("Class_", '', test.y)
test.y = as.integer(test.y) - 1

x <- rbind(data.train[, -ncol(data.train)], data.test[, -ncol(data.test)])
x <- as.matrix(x)
nrow = nrow(x)
ncol = ncol(x)
x = as.matrix(as.numeric(x), nrow = nrow, ncol = ncol)
x <- matrix(x, nrow = nrow , ncol = ncol)
train.idx <- 1:nrow(data.train)
test.idx <- (nrow(data.train) + 1 ) : nrow(x)

param <- list("objective" = "multi:softprob", "eval_metric" = "mlogloss", "num_class" = 9, "nthread" = 8)
cv.nround = 50
bst.cv = xgb.cv(param = param, data = x[train.idx,], label = y, nfold = 3, nrounds = cv.nround)
nround = 35
bst = xgboost(param = param, data = x[train.idx,], label = y, nrounds = nround)

pred = predict(bst, x[test.idx,])
pred = matrix(pred, 9, length(pred) / 9)
pred = t(pred)

indicator.matrix <- function(y){
  y <- as.factor(y)
  k <- length(unique(y))
  n <- length(y)
  res <- matrix(0, nrow = n, ncol = k)
  for(i in 1:n){
    res[i, as.integer(y[i])] <- 1
  }
  return(res)
}

mlogloss <- function(y, prob){
  ncol <- ncol(y)
  nrow <- nrow(y)
  
  sum <- 0
  for(i in 1:nrow){
    for(j in 1:ncol){
      sum <- sum + y[i,j]*log(max(prob[i,j], 10^(-15)))
    }
  }
  sum <- - sum / nrow
  return(sum)
}
test.y.mat <- indicator.matrix(test.y)
mlogloss(test.y.mat, pred)
