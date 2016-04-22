#QUESTION 2


X=matrix(0,1000,3)
#preparing the dataset
#initializing the value of size parameter.
Q=matrix(c(0.8,0.2,0.5,0.5,0.2,0.8),nrow=2,ncol=3)


pie=c(0.3,0.7)


#Question 2a
for(i in 1:1000)
{
  a=runif(1,0,1)
  if(a<=pie[1])
  {
    b=runif(3,0,1)
    if(b[1]<=0.8)
      X[i,1]=1
    else
      X[i,1]=0
  
    if(b[2]<=0.5)
      X[i,2]=1
    else
      X[i,2]=0
    if(b[3]<=0.2)
      X[i,3]=1
    else
      X[i,3]=0
  }
  else
  {
    b=runif(3,0,1)
    if(b[1]<=0.2)
      X[i,1]=1
    else
      X[i,1]=0
  
    if(b[2]<=0.5)
      X[i,2]=1
    else
      X[i,2]=0
    if(b[3]<=0.8)
      X[i,3]=1
    else
      X[i,3]=0
  }

}


#Question 2b Expectation Maximization
#initital pi
pi=c(0.5,0.5)
N=dim(X)[1]
#initial Q
#Q=matrix(c(0.6,0.4,0.5,0.5,0.5,0.5),nrow=2,ncol=3)
Q=matrix(c(0.5,0.5,0.5,0.5,0.5,0.5),nrow=2,ncol=3)
ll=NULL
for(i in 1:10)
{
  
  prob1=pi[1]*((Q[1,1]^X[,1])*((1-Q[1,1])^(1-X[,1]))*(Q[1,2]^X[,2])*((1-Q[1,2])^(1-X[,2]))*(Q[1,3]^X[,3])*((1-Q[1,3])^(1-X[,3])))
  prob2=pi[2]*((Q[2,1]^X[,1])*((1-Q[2,1])^(1-X[,1]))*(Q[2,2]^X[,2])*((1-Q[2,2])^(1-X[,2]))*(Q[2,3]^X[,3])*((1-Q[2,3])^(1-X[,3])))
  
  ll[i]=sum(log(prob1+prob2))
  print(paste("loglikelihood=",ll[i]))
  
  #EXPECTATION
  resp_1=prob1/(prob1+prob2)
  resp_2=prob2/(prob1+prob2)
  
  N1=sum(resp_1)
  N2=sum(resp_2)
  
  #MAXIMIZATION
  #new pi's
  
  pi[1]=N1/N
  pi[2]=N2/N
  
  #new q's
  q1=colSums(resp_1*X)/N1
  q2=colSums(resp_2*X)/N2
  
  Q[1,]=q1
  Q[2,]=q2
  
}

#QUESTION 2c
#ON SETTING THE Q VALUES TO BE CONSTANT THE VALUE OF PRIORS DOES NOT CHANGE,IT REMAINS CONSTANT. 

