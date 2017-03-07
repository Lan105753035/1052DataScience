########################
# homework1 example
########################
library(tools)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript homework1_105753035.R  -files test.1.csv -out result.csv", call.=FALSE)
} else if (length(args)>0) {
  i<-1 
  while(i < length(args))
  {
    if(args[i] == "-files"){
      i_f<-args[i+1]
      i<-i+1
    }else if(args[i] == "-out"){
      o_f<-args[i+1]
      i<-i+1
    }else{
      stop(paste("Unknown flag", args[i]), call.=FALSE)
    }
    i<-i+1
  }
}

  sampleTable <- read.csv(i_f,row.names=1)
  set <- file_path_sans_ext(i_f)

  weight <- max(sampleTable$weight)
  weight <- round(weight,digits = 2)
  height <- max(sampleTable$height)
  height <- round(height,digits = 2)


  x <- data.frame(set,weight,height)
  write.csv(x,file = o_f,row.names = F)
