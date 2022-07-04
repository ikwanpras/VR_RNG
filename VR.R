VR<-function(a,z0,c,m)
{
  z<-rep(0,20)
  R<-rep(0,20)
  x<-rep(0,20)
  for(i in 1:20)
{
z[i]<-((a*z0)+c) %% m
z0<-z[i]
R[i]<-z[i]/m

if(0<R[i] & R[i]<=0.167)
{ x[i]=1}
else if (0.167<R[i] & R[i]<=0.333)
{ x[i]=2}
else if (0.333<R[i] & R[i]<=0.5)
{ x[i]=3}
else if (0.5<R[i] & R[i]<=0.67)
{ x[i]=4}
else if (0.67<R[i] & R[i]<=0.83)
{ x[i]=5}
else if (0.83<R[i] & R[i]<=1)
{ x[i]=6}
}
print(z)
cat("nilai Random Number : \n",R)
cat("\n nilai Random Variate Diskrit : \n",x,"\n")
}
VR(19,12357,237,128)
