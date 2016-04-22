#QUESTION 5a


#ALPHA= 0.0001 for maximum accuracy.
#ACCURACY is 64.08%

X=balance_scale
X[,1]=as.character(X[,1])
Y=X[,1]

#Dummy-encoding
library(caret)
dummy=dummyVars(~.,data=X)
X1<- data.frame(predict(dummy, newdata = X))


X=X1
X=as.matrix(X)

#weight vectors for 3 labels.
W_B=matrix(0,nrow=4,ncol=1)
W_L=matrix(0,nrow=4,ncol=1)
W_R=matrix(0,nrow=4,ncol=1)
W=matrix(0,nrow=4,ncol=1)

#function to calculate softmax
FUN<-function(X,W)
{
  numerator=exp(X%*%W)
  denominator=exp(X%*%W_B)+exp(X%*%W_L)+exp(X%*%W_R)
  return(numerator/denominator)
  
}

m=dim(X)[1]

#GRADIENT DESCENT
alpha=as.numeric(readline("ENTER THE VALUE OF LEARNING RATE(ALPHA)     "))

for(i in 1:m)
{
W_B[1]=W_B[1]+alpha*sum(as.numeric((X[i,1]-FUN(X[i,4:7],W_B)))*X[,4])
W_B[2]=W_B[2]+alpha*sum(as.numeric((X[i,1]-FUN(X[i,4:7],W_B)))*X[,5])
W_B[3]=W_B[3]+alpha*sum(as.numeric((X[i,1]-FUN(X[i,4:7],W_B)))*X[,6])
W_B[4]=W_B[4]+alpha*sum(as.numeric((X[i,1]-FUN(X[i,4:7],W_B)))*X[,7])

W_L[1]=W_L[1]+alpha*sum(as.numeric((X[i,2]-FUN(X[i,4:7],W_L)))*X[,4])
W_L[2]=W_L[2]+alpha*sum(as.numeric((X[i,2]-FUN(X[i,4:7],W_L)))*X[,5])
W_L[3]=W_L[3]+alpha*sum(as.numeric((X[i,2]-FUN(X[i,4:7],W_L)))*X[,6])
W_L[4]=W_L[4]+alpha*sum(as.numeric((X[i,2]-FUN(X[i,4:7],W_L)))*X[,7])

W_R[1]=W_R[1]+alpha*sum(as.numeric((X[i,3]-FUN(X[i,4:7],W_R)))*X[,4])
W_R[2]=W_R[2]+alpha*sum(as.numeric((X[i,3]-FUN(X[i,4:7],W_R)))*X[,5])
W_R[3]=W_R[3]+alpha*sum(as.numeric((X[i,3]-FUN(X[i,4:7],W_R)))*X[,6])
W_R[4]=W_R[4]+alpha*sum(as.numeric((X[i,3]-FUN(X[i,4:7],W_R)))*X[,7])

}


Y1=NULL
for(i in 1:m)
{
  v=c(FUN(X[i,4:7],W_B),FUN(X[i,4:7],W_L),FUN(X[i,4:7],W_R))
  ma=match(max(v),v)
  if(ma==1)
    Y1[i]="B"
  if(ma==2)
    Y1[i]="L"
  else
    Y1[i]="R"
}
print(paste("THE ACCURACY IS",sum(as.numeric(Y1==Y))/6.25,"%",sep=" "))


