RNG<-function(a,z0,c,m)
{
  z<-rep(0,15)
  R<-rep(0,15)
  X<-rep(0,15)
  for(i in 1:15)
  {
    z[i]<-(a*z0)%% m
    z0<-z[i]
    R[i]<-z[i]/m
    X[i]<-((1/2)*R[i])
  }
  print(z)
  cat("nilai Random Number : \n",R)
  cat("\n nilai Random Variate Kontiniu : \n",X,"/n")
  rata2<-mean(X)
  cat("rata-rata=",rata2,"\n")
}
RNG(197,12357,237,128)