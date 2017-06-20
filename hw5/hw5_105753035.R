library(arules)
library(boot)
library(caret)
library(nnet)

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw5_105753035.R --fold n --out performance.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    n_folds<-args[i+1]
    i<-i+1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("n-fold :", n_folds))
print(paste("output file:", out_f))
n_folds<-as.numeric(n_folds)

#load data
d <- read.csv("Archaeal_tfpssm.csv",header=F)
head(d[,5600:5603])

#preprocessing
d_use <- d[,1:5602]
#d_use$V2 <- as.numeric(d_use$V2)

x5 <- sample(1:805, 80, replace=F)
d_test <- d_use[x5,]

#Generalized Linear Models
ctrl <- trainControl(method = "cv", number = n_folds, savePredictions = TRUE)

model <- multinom(V2~V3+V4+V5+V6+V7+V8+V9+V10,data = d_use)
cv.model<- train(V2~V3+V4+V5+V6+V7+V8+V9+V10,data = d_use,method="multinom",trControl = ctrl)

train_pred <- predict(model,newdata=d_use)
cv.train_pred <- predict(cv.model,newdata=d_use)
cv.test_pred <- predict(cv.model,newdata=d_test)

#performance

trainCM <- confusionMatrix(data=train_pred,d_use$V2)
trainCM$overall[1]
cv.trainCM <- confusionMatrix(data=cv.train_pred,d_use$V2)
cv.trainCM$overall[1]
cv.testCM <- confusionMatrix(data=cv.test_pred,d_test$V2)
cv.testCM$overall[1]

#output result

out_data<-data.frame("set" = character(3), "accuracy" = numeric(3),stringsAsFactors=FALSE)
out_data<-rbind(out_data[1,],c("training",trainCM$overall[1]))
out_data<-rbind(out_data,c("calibration",cv.testCM$overall[1]))
out_data<-rbind(out_data,c("test",cv.testCM$overall[1]))
out_data <- out_data[-1,]
#output file
write.csv(out_data, file=out_f, row.names = F, quote = F)
