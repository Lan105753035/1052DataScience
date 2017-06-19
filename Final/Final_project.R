library(arules)
library(boot)
library(caret)
library('ggplot2')

#load data
d <- read.csv("NBA_MVP.csv",header=T)
test <- read.csv("NBA_MVP_test.csv",header = T)
data2017 <- read.csv("NBA_MVP_2017.csv",header = T)


#preprocessing


#Generalized Linear Models
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

model <- train(Pts.Won~G+MP+PTS+TRB+AST+STL+BLK+FG.+X3P.+FT.+Pts.Max,data = d,method="lm",trControl = ctrl, tuneLength = 5)

modelnew <- train(Pts.Won~PTS+AST+FG.,data = d,method="lm",trControl = ctrl, tuneLength = 5)

null<-nullModel(d$Pts.Won,340)


train_pred <- predict(modelnew,newdata=d)
test_pred <- predict(modelnew,newdata=test)
null_pred <- predict(null,newdata=test)
mvp2017_pred <-predict(modelnew,newdata=data2017)

#r-squre
rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }

trainout<-rsq(d$Pts.Won , train_pred)

testout<-rsq(test$Pts.Won , test_pred)

nullout<-rsq(test$Pts.Won , null_pred)

#plot result

ggplot(data=test,aes(x=test_pred,y=test$Pts.Won)) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=test_pred,
                  y=test$Pts.Won),color="black") +
  geom_line(aes(x=test$Pts.Won,
                y=test$Pts.Won),color="blue",linetype=2) 

#root mean square error 

rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }
rmse(d$Pts.Won,train_pred)
rmse(test$Pts.Won,test_pred)


