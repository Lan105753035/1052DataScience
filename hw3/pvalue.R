MAX<-"method3.csv"

A<-read.csv(MAX)
B<-read.csv("method5.csv")

d<-data.frame(A=A$prediction,B=B$prediction)
tab <- table(d)
print(tab)
out<-fisher.test(tab)
print(out$p.value)
