# Author: Jose Mijangos
# Date: April 7, 2018
# Course: Introduction to Data Science
# Abstract: K Nearest Neighbor with applications to college data
#

# returns a knn classifier
knn_create = function(dat, labels) {
  return (list(dat=dat, labels=labels))
}

# returns the mode of a given vector
vmode = function(x) {
  return (names(sort(table(x), decreasing = TRUE))[1])
}

#returns the labels correspond to the k smallest values in a given vector
smallest_labels = function(x, labels, k) {
  return (labels[order(x)[1:k]])
}

# returns a matrix such that matrix[i,j] gives the distance between the ith row of x and the jth row of y
distances_between = function(x, y) {
  m = as.numeric(nrow(x))
  n = as.numeric(nrow(y))
  z = dist(rbind(x, y))
  matrix = data.frame(V1 = c(1:m))
  count = 0
  col = 1;
  for(i in m:(m+n-1)){
    v=c()
    num = 0
    for(j in (m+n-2):(n)){
      v=c(v, i+num)
      num = num + j
    }
    v = c(v, i + num)
    matrix[col] = c(z[v])
    col = col + 1
  }
  return(matrix)
}

# returns the predicted labels of x using a k nearest neighbor classifier
knn_predict = function(knn, x, k = 5) {
  matrix = distances_between(knn$dat, x)
  kNearest = apply(matrix, 2, function(x) smallest_labels(x, knn$labels, k))
  if(k==1){
    return(print("Setting k to 1 is not recommended"))
  }
  return(apply(kNearest, 2, vmode))
}

# Using a knn classifier, predict which colleges are private
predict_private = function(dat, dependent, features, k) {
  # make first column the row names
  rownames(dat) = dat[,1]
  dat = dat[,-1]
  
  # scale all features
  dat[,2:ncol(dat)] = scale(dat[,2:ncol(dat)])
  
  # randomize data before creating training and test sets
  set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
  dat = dat[sample(1:nrow(dat)),]
  
  # number of training examples
  n = floor(nrow(dat)*.80)
  
  # feature vectors training and testing data
  fvs = dat[,features]
  tr_dat = fvs[1:n,]
  te_dat = fvs[-(1:n),]
  
  # labels for training and test data
  labels = dat[,dependent]
  tr_labels = labels[1:n]
  te_labels = labels[-(1:n)]
  
  # get predictions
  knn = knn_create(tr_dat, tr_labels)
  predicts = knn_predict(knn, te_dat, k)
  
   # create confusion matrix
  actuals = te_labels
  tbl = table(actuals, predicts[1:length(predicts)])
  
  # display error rate and confusion matrix
  cat("Error Rate: = ",substr(as.character(1-sum(as.vector(actuals)==as.vector(predicts))/length(actuals)),1,7))
  return(tbl)
}

# Test with Crash data
#datCrash = read.csv("C:\\Users\\josem\\Desktop\\Crash.csv")
#dependent = c("Cross.Street.Type")
#features = c("Longitude", "Latitude")
#dat=datCrash[1:5000,c(dependent,features)]
#predict_private(dat, dependent, features, k=5)

# Test with College data
dat = read.csv("https://raw.githubusercontent.com/grbruns/cst383/master/College.csv")
dependent = c("Private")
features = c("Expend", "Outstate", "Enroll","Room.Board","F.Undergrad")

install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

par(mfrow=c(1,2))
par(mar=c(2,2,6,2))
colors <- c("red", "blue")
colors <- colors[as.numeric(dat$Private)]
scatterplot3d(dat[,c("Expend", "Room.Board","F.Undergrad")],color=colors,pch=20)
legend(c(9,9), col= c("blue","red"), bg="grey", lty=c(1,1), lwd=2, xjust = 1, yjust=0, legend = c("Public", "Private"), cex = 1.1)
scatterplot3d(dat[,c("Expend", "Outstate", "Enroll")],color=colors,pch=20)
legend(c(9,9), col= c("blue","red"), bg="grey", lty=c(1,1), lwd=2, xjust = 1, yjust=0, legend = c("Public", "Private"), cex = 1.1)

predict_private(dat, dependent, features, k=5)
