library(plotrix)
source("round.R")

kalium1=read.table("../data/Kalium1.txt",dec=",")

x=kalium1$V1
y=kalium1$V3
t=kalium1$V2
sy=sqrt(y/t)
y_corr=y-u[23:31]
sy_corr=sqrt(sy^2+su[23:31]^2)

plotCI(x,y_corr,ui=sy_corr+y_corr,li=(y_corr-sy_corr)*(y_corr>sy_corr)+0.0001*(y_corr<=sy_corr),cex=0.6,pch=4,bty="l",log="y",xlab="U / V",ylab=expression(n / s^-1),ylim=c(min(y_corr*(y_corr>0)+100*(y_corr<=0)),max(y)+0.5),col="blue")
plotCI(x,y,uiw=sy,cex=0.6,pch=4,bty="l",log="y",ylim=c(2,max(y)+0.5),add=TRUE)
grid()

########################

untergrund2=read.table("../data/UntergrundNacht.txt",dec=",")[46:90,]

Uu=sum(untergrund2$V1)/length(untergrund2$V1)
nu=sum(untergrund2$V3)/length(untergrund2$V3)
tu=sum(untergrund2$V2)
snu=sqrt(nu/tu)

n=c()
sn=c()

mges=c(3.0649,2.6914,2.7471,2.6400,2.7814,3.0037,2.9154,2.9377,2.5466)
ms=c(1.3260,1.2899,1.3255,1.2899,1.3255,1.2899,1.3255,1.2899,1.3255)
smeinzel=0.0001

m=(mges-ms)*10^-3
sm=sqrt(2)*smeinzel*10^-3

for(i in 1:9){
kalium=read.table(paste("../data/Kalium",(i+1),".txt",sep=""),dec=",")
U=sum(kalium$V1)/length(kalium$V1)
nges[i]=sum(kalium$V3)/length(kalium$V3)
t=sum(kalium$V2)
snges[i]=sqrt(nges[i]/t)
} 
n=nges-nu
sn=sqrt(snges^2+snu^2)

fitfunc <- y~a*(1-exp(-b*x))
a0<-5.4
b0<-1000
fit<-nls(fitfunc,data.frame(x=m,y=n),weigths=1/sn^2,start=list(a=a0,b=b0))

a<-summary(fit)$coefficients["a","Estimate"]
b<-summary(fit)$coefficients["b","Estimate"]
sa<-summary(fit)$coefficients["a","Std. Error"]
sb<-summary(fit)$coefficients["b","Std. Error"]


plotCI(m,n,uiw=sn,err="y",cex=0.6,pch=4,bty="l",xlab="m / kg",ylab=expression(n / s^-1),xlim=c(0.001,0.002),ylim=c(0,5.5))
plotCI(m,n,uiw=sm,err="x",cex=0.6,pch=4,add=TRUE)
grid()

plot(function(x){a*(1-exp(-b*x))},0,0.002,col="red",add=TRUE)

Na=6.022*10^23
fB=1.29
Omega=2*pi
hrel=0.000118

Thalb=log(2)*Na*Omega*fB/4/pi/a*hrel/b/1.12/0.07455
ThalbJahre=Thalb/3600/24/365.25
sThalbJahre=ThalbJahre*sqrt((sa/a)^2+(sb/b)^2+2*(summary(fit,correlation=TRUE)$correlation[1,2])*sa*sb/a/b)

results=roundfunc(c(ThalbJahre,sThalbJahre)/10^9)

cat("T_(1/2) = (",results[1]," +- ",results[2],")*10^9 Jahre",sep="")