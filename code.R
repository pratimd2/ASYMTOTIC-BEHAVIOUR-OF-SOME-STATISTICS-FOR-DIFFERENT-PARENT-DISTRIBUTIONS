rm(list=ls())
F=function(n)
{
 
a=c(rnorm(n,5,2))
a
b=mean(a)
return(b)
}
xbar=array(dim=1)
n=seq(1:1000)
n
 for(i in n)
{
 xbar[i]=F(i)
 
}
 plot(n,xbar,'l')

wlln<-function(n,eps,k = 100)
{
  x = rexp(n*k,rate = 1)
  M = matrix(x,nr = k)
  X = apply(M,1,'median')
  t = sum(abs(X - log(2)) < eps)/k
  return(t)
}

#This is a quiet slow code because traking 
#convergence in probability is in itself difficult
set.seed(50)

#epsilon = 0.05
prob2=NULL
ind = seq(1,10000,100)
for (n in 1:length(ind))
  prob2[n]=wlln(n = ind[n],eps = 0.05,k = 100)
plot(ind,prob2,type="l",xlab="n",ylab="probability",main=expression(epsilon~ "= 0.05"))

rm(list=ls())
f=function(n,R)
{
 x1=array(dim=1)
 xn=array(dim=1)
 midrange=array(dim=1)
 xmed=array(0)
 xbar=array(0)
 b=array(0)
 
 
 for(i in 1:R )
{b=rnorm(n,0,1)
 x1[i]=min(b)
 xn[i]=max(b)
 midrange[i]=(xn[i]+x1[i])/2
 xmed[i]=median(b)
 xbar[i]=mean(b)
}
print(max(x1))
#br1=seq(min(x1),max(x1),length=10)
#h1=hist(x1,breaks=br1,freq=FALSE,xaxt="n",ylim=c(0,100),main=bquote(bold(n==.(n))))
#axis(1,at=br1,round(br1,3))
#br2=seq(min(xn),max(xn),length=10)
#h2=hist(xn,breaks=br2,freq=FALSE,xaxt="n",ylim=c(0,5),main=bquote(bold(n==.(n))))
#axis(1,at=br2,round(br2,3))
#br3=seq(min(midrange),max(midrange),length=10)
#h3=hist(midrange,breaks=br3,freq=FALSE,xaxt="n",ylim=c(0,5),main=bquote(bold(n==.(n))))
#axis(1,at=br3,round(br3,3))
#br4=seq(min(xmed),max(xmed),length=10)
#h4=hist(xmed,breaks=br4,freq=FALSE,xaxt="n",ylim=c(0,150),main=bquote(bold(n==.(n))))
#axis(1,at=br4,round(br4,3))
#curve(dnorm(x),from=0,to=3,add=T)
br5=seq(min(xbar),max(xbar),length=10)
h5=hist(xbar,breaks=br5,freq=FALSE,xaxt="n",ylim=c(0,15),main=bquote(bold(n==.(n))))
axis(1,at=br5,round(br5,3))

}
  
 par(mfrow=c(3,1))
f(100,1000)
f(500,1000)
f(1000,1000)


rm(list=ls())
f=function(n,R)
{
 x1=array(dim=1)
 xn=array(dim=1)
 midrange=array(dim=1)
 xmed=array(0)
 xbar=array(0)
 b=array(0)
 
 
 for(i in 1:R )
{b=rexp(n,rate=1)
 x1[i]=min(b)
 xn[i]=max(b)
 midrange[i]=(xn[i]+x1[i])/2
 xmed[i]=median(b)
 xbar[i]=mean(b)
}
print(max(x1))
#br1=seq(min(x1),max(x1),length=10)
#h1=hist(x1,breaks=br1,freq=FALSE,xaxt="n",ylim=c(0,100),main=bquote(bold(R==.(R))))
#axis(1,at=br1,round(br1,3))
#br2=seq(min(xn),max(xn),length=10)
#h2=hist(xn,breaks=br2,freq=FALSE,xaxt="n",ylim=c(0,5),main=bquote(bold(n==.(n))))
#axis(1,at=br2,round(br2,3))
br3=seq(min(midrange),max(midrange),length=10)
h3=hist(midrange,breaks=br3,freq=FALSE,xaxt="n",ylim=c(0,1),main=bquote(bold(n==.(n))))
axis(1,at=br3,round(br3,3))
#br4=seq(min(xmed),max(xmed),length=10)
#h4=hist(xmed,breaks=br4,freq=FALSE,xaxt="n",ylim=c(0,150),main=bquote(bold(n==.(n))))
#axis(1,at=br4,round(br4,3))
#curve(dnorm(x),from=0,to=3,add=T)
#br5=seq(min(xbar),max(xbar),length=10)
#h5=hist(xbar,breaks=br5,freq=FALSE,xaxt="n",ylim=c(0,15),main=bquote(bold(n==.(n))))
#axis(1,at=br5,round(br5,3))

}
  
 par(mfrow=c(3,1))
f(1000,100)
f(1000,500)
f(1000,1000) 


