#Problem 1

sChartData=read.csv(file=file.choose())

xbarbar=mean(sChartData$Mean)
sbar=mean(sChartData$SD)

A3=1.427 #Looked up from table for n=5
B3=0
B4=2.089


#s chart
sCL=sbar
sUCL=B4*sbar
sLCL=B3*sbar

plot(sChartData$SD,xlab="Sample",ylim=c(0.95*sLCL,1.05*sUCL),ylab="Sample standard deviation",main="Phase I")
lines(rep(sCL,length(sChartData$SD)),col="blue")
lines(rep(sLCL,length(sChartData$SD)),col="red")
lines(rep(sUCL,length(sChartData$SD)),col="red")



#xbar chart
xbarbar=mean(sChartData$Mean)
sbar=mean(sChartData$SD)

XCL=xbarbar
XUCL=xbarbar+A3*sbar
XLCL=xbarbar-A3*sbar

plot(sChartData$Mean,ylim=c(0.9999*XLCL,1.0003*XUCL),xlab="Sample",ylab="Sample mean",main="Phase I")
lines(rep(XCL,length(sChartData$Mean)),col="blue")
lines(rep(XLCL,length(sChartData$Mean)),col="red")
lines(rep(XUCL,length(sChartData$Mean)),col="red")

#Remove data point that is out of control
sChartData$Mean[9]=NA
sChartData$SD[9]=NA

xbarbar=mean(sChartData$Mean,na.rm=TRUE)
sbar=mean(sChartData$SD,na.rm=TRUE)




#s chart
sCL=sbar
sUCL=B4*sbar
sLCL=B3*sbar

plot(sChartData$SD,xlab="Sample",ylim=c(0.95*sLCL,1.05*sUCL),ylab="Sample standard deviation",main="Phase I")
lines(rep(sCL,length(sChartData$SD)),col="blue")
lines(rep(sLCL,length(sChartData$SD)),col="red")
lines(rep(sUCL,length(sChartData$SD)),col="red")


XCL=xbarbar
XUCL=xbarbar+A3*sbar
XLCL=xbarbar-A3*sbar

plot(sChartData$Mean,ylim=c(0.9999*XLCL,1.0003*XUCL),xlab="Sample",ylab="Sample mean",main="Phase I")
lines(rep(XCL,length(sChartData$Mean)),col="blue")
lines(rep(XLCL,length(sChartData$Mean)),col="red")
lines(rep(XUCL,length(sChartData$Mean)),col="red")
