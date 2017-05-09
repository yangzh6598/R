phaseIdata=read.csv(file=file.choose())

phaseIdata$xbar=apply(phaseIdata[,2:6],1,mean) #Calculates sample averages and creates as new column
temp=apply(phaseIdata[,2:6],1,range) #Calculates sample ranges, returns min and max
phaseIdata$R=t(temp)[,2]-t(temp)[,1] #Creates R column as range

xbarbar=mean(phaseIdata$xbar)
Rbar=mean(phaseIdata$R)

A2=0.577 #Looked up from table for n=5
D3=0
D4=2.114


#R chart
RCL=Rbar
RUCL=D4*Rbar
RLCL=D3*Rbar

plot(phaseIdata$R,ylim=c(0.91*RLCL,1.5*RUCL),xlab="Sample",ylab="Sample range")
lines(rep(RCL,length(phaseIdata$xbar)),col="blue")
lines(rep(RLCL,length(phaseIdata$xbar)),col="red")
lines(rep(RUCL,length(phaseIdata$xbar)),col="red")

#Remove data point that is out of control
phaseIdata[16,7]=NA
phaseIdata[16,8]=NA

xbarbar=mean(phaseIdata$xbar,na.rm=TRUE)
Rbar=mean(phaseIdata$R,na.rm=TRUE)

RCL=Rbar
RUCL=D4*Rbar
RLCL=D3*Rbar

plot(phaseIdata$R,ylim=c(0.91*RLCL,1.5*RUCL),xlab="Sample",ylab="Sample range")
lines(rep(RCL,length(phaseIdata$xbar)),col="blue")
lines(rep(RLCL,length(phaseIdata$xbar)),col="red")
lines(rep(RUCL,length(phaseIdata$xbar)),col="red")



#xbar chart
XCL=xbarbar
XUCL=xbarbar+A2*Rbar
XLCL=xbarbar-A2*Rbar

plot(phaseIdata$xbar,ylim=c(0.91*XLCL,1.1*XUCL),xlab="Sample",ylab="Sample mean")
lines(rep(XCL,length(phaseIdata$xbar)),col="blue")
lines(rep(XLCL,length(phaseIdata$xbar)),col="red")
lines(rep(XUCL,length(phaseIdata$xbar)),col="red")


###Phase II
phaseIIdata=read.csv(file=file.choose())

phaseIIdata$xbar=apply(phaseIIdata[,2:6],1,mean) #Calculates sample averages and creates as new column
temp=apply(phaseIIdata[,2:6],1,range) #Calculates sample ranges, returns min and max
phaseIIdata$R=t(temp)[,2]-t(temp)[,1] #Creates R column as range

plot(phaseIIdata$R,ylim=c(0.91*RLCL,1.1*RUCL),xlab="Sample",ylab="Sample range")
lines(rep(RCL,length(phaseIIdata$xbar)),col="blue")
lines(rep(RLCL,length(phaseIIdata$xbar)),col="red")
lines(rep(RUCL,length(phaseIIdata$xbar)),col="red")


plot(phaseIIdata$xbar,ylim=c(0.91*XLCL,1.1*XUCL),xlab="Sample",ylab="Sample mean")
lines(rep(XCL,length(phaseIdata$xbar)),col="blue")
lines(rep(XLCL,length(phaseIdata$xbar)),col="red")
lines(rep(XUCL,length(phaseIdata$xbar)),col="red")

