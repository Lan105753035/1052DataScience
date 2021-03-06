library('ROCR')
query_func<-function(query_m, i)
{
  TP<-0
  TN<-0
  FP<-0
  FN<-0
  cM <- table(truth=d$reference, prediction=d$prediction)
  if(query_m == "male"){
    TP<-cM[2,2]
    TN<-cM[1,1]
    FP<-cM[1,2]
    FN<-cM[2,1]
  }
  else if (query_m == "female") {
    TP<-cM[1,1]
    TN<-cM[2,2]
    FP<-cM[2,1]
    FN<-cM[1,2]
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
  eval <- prediction(d$pred.score,d$reference)
  sensitivity<<-round (TP/(TP+FN),digits = 2)
  specificity<<-round (TN/(TN+FP),digits = 2)
  precision<<-round (TP/(TP+FP),digits = 2)
  F1<<-round (2*precision*sensitivity/(precision+sensitivity),digits = 2)
  AUC <<- round(attributes(performance(eval,'auc'))$y.values[[1]], digits = 2)
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw3_105753035.R --target male|female --files file1 file2 ... filen --out out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("--", c(args[(i+1):length(args)], "--"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
names<-c()
sens<-c()
spes<-c()
f1s<-c()
AUCs<-c()
for(file in files)
{
  #name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  query_func(query_m, d)
  sens<-c(sens, sensitivity)
  spes<-c(spes, specificity)
  f1s<-c(f1s, F1)
  AUCs<-c(AUCs, AUC)
  names<-c(names,file)
}
F1.sorted<-sort(f1s)#get the sorted vector
len<-length(f1s)
value.max<-F1.sorted[len]###This is the max
second.max<-F1.sorted[(len-1)]###the value smaller only than max


out_data<-data.frame(method=names, sensitivity=sens, specificity=spes, F1=f1s, AUC=AUCs, stringsAsFactors = F)
index<-sapply(out_data[c("sensitivity","specificity","F1","AUC")],which.max)

#find MAX F1
index2<-sapply(out_data[c("F1")],which.max)
A<-out_data[c("method")]
A.method<-A[index2,]
A.csv<-paste0(A.method,".csv")
print(paste(A.method,":",value.max))

#find second MAX F1
out_data2<-out_data[-index2,]
index3<-sapply(out_data2[c("F1")],which.max)
B<-out_data2[c("method")]
B.method<-B[index3,]
B.csv<-paste0(B.method,".csv")
print(paste(B.method,":",second.max))

#calculate p-value
A<-read.csv(A.method)
B<-read.csv(B.method)
d<-data.frame(A=A$prediction,B=B$prediction)
tab <- table(d)
print(tab)
out<-fisher.test(tab)
print(paste("p-value:",round(out$p.value,digits = 5)))

#modify methods names
newnames<-c()
for (name in names)
{
  name<-gsub(".csv", "", basename(name))
  newnames<-c(newnames,name)
}


out_datanew<-data.frame(method=newnames, sensitivity=sens, specificity=spes, F1=f1s, AUC=AUCs, stringsAsFactors = F)


#add * if p-value < 0.05
if (out$p.value < 0.05){
  A.method<-gsub(".csv", "", basename(A.method))
  A.methodstar<-paste0(A.method,"*")
  addstar<-replace(newnames[index],3,A.methodstar)
  out_datanew<-rbind(out_datanew,c("highest",addstar))
}else {
  out_datanew<-rbind(out_datanew,c("highest",newnames[index]))
}

# output file
write.csv(out_datanew, file=out_f, row.names = F, quote = F)



