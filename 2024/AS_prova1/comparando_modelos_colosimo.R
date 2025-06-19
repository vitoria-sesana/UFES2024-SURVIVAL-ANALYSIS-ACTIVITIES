 require(survival)
 require(flexsurv)
 
 tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
 cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)
 
 ajust1<-survreg(Surv(tempos,cens)~1,dist="exponential")
 ajust1
 alpha<-exp(ajust1$coefficients[1])
 alpha
 
 ajust2<-survreg(Surv(tempos,cens)~1,dist="weibull")
 ajust2
 
 alpha<-exp(ajust2$coefficients[1])
 
 gama<-1/ajust2$scale
 cbind(gama, alpha)
 
 
 
 ajust3<-survreg(Surv(tempos,cens)~1,dist="lognorm")
 ajust3$loglik[2]

ajust4 <- survreg(Surv(tempos,cens)~1,dist="lognorm")

ajust4$

 ajust3<- flexsurvreg(Surv(tempos,cens)~1,dist="lognorm")
 
 ajust3$coefficients
ajust3$res
  require(survival)
  tempos<- c(1,2,3,3,3,5,5,16,16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16)
  cens<-c(0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0)
  grupos<-c(rep(1,15),rep(2,14))
  ekm<- survfit(Surv(tempos,cens)~grupos)
 
 
  time<-ekm$time
  st<-ekm$surv
  ste<- exp(-time/20.41)
  stw<- exp(-(time/21.34)^1.54)
  stln<- pnorm((-log(time)+ 2.72)/0.76)
  stll<-1 / (1+(time/alpha)^lambda)

  cbind(time,st,ste,stw,stln, stll)
  
  
   par(mfrow=c(1,3))
   plot(ste,st,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), ylab = "S(t): Kaplan-Meier",
          xlab="S(t): exponencial")
   lines(c(0,1), c(0,1), type="l", lty=1)
   plot(stw,st,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), ylab = "S(t): Kaplan-Meier",
          xlab="S(t): Weibull")
   lines(c(0,1), c(0,1), type="l", lty=1)
   plot(stln,st,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), ylab = "S(t): Kaplan-Meier",
          xlab="S(t): log-normal")
   lines(c(0,1), c(0,1), type="l", lty=1)

   
  