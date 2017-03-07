
library(readr)
test_1 <- read_csv("C:/Users/USER/Desktop/Data Science/test.1.csv")
weight <- max(test_1$weight)
height <- max(test_1$height)
result<-matrix( c(weight,height),nrow=2,ncol=1,byrow = TRUE)
print(result)
matri