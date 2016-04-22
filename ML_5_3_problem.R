#QUESTION 3

vec=seq(0,10,0.5)

dis=function(w,x)
{
  Q=c(1,x,x^2)
  z=sum(w*Q)
  
  y=1/(1+(exp(-z)))
  return(y)
}


#3a
w0=3;w1=-.05;w2=-.08
w=c(w0,w1,w2)
y=NULL
for(i in vec)
{
  y[i]=dis(w,i)
}
plot(y,type='l',main="Probability of person joining queue",xlab = "Length of line",ylab="Probability")

X=chipotle
dis1=function(w,i)
{
Q=c(1,X[i,1],(X[i,1]^2))
  z=sum(w*Q)
  
y=1/(1+(exp(-z)))
return(y)
}

#3b
w=c(0,0,0)

alpha=readline("enter step size ")
alpha=as.numeric(alpha)
#alpha=0.0001
for(i in 1:dim(chipotle)[1])
{
  Q=c(1,X[i,1],(X[i,1]^2))
  w=w+(alpha*(X[i,2]-dis1(w,i))*Q)
}

#ESTIMATED W for alpha=0.0001
#0.7085809111  0.8995567787 -0.1714233179


#To find sum of squared error for each set of weights
u=NULL

for(i in vec)
{
  u=append(u,dis(w,i))
}
plot(vec,u,type='l',main="Probability of person joining queue",xlab = "Length of line",ylab="Probability")

