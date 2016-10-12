source("round.R")

samarium1=read.table("../data/Samarium1.txt",dec=",")

x=samarium1$V1
y=samarium1$V3
t=samarium1$V2
sy=sqrt(y/t)

y_corr=y-u[16:31]
sy_corr=sqrt(sy^2+su[16:31]^2)

plotCI(x,y_corr,ui=sy_corr+y_corr,li=(y_corr-sy_corr)*(y_corr>sy_corr)+0.0001*(y_corr<=sy_corr),cex=0.6,pch=4,bty="l",log="y",xlab="U / V",ylab=expression(n / s^-1),ylim=c(min(y_corr*(y_corr>0)+100*(y_corr<=0)),max(y)),col="blue")
plotCI(x,y,uiw=sy,cex=0.6,pch=4,bty="l",log="y",xlab="U / V",ylab="n / Hz",add=TRUE)
grid()

########################

Rrho=4.025*10^(-2)
Na=6.022*10^23
hrel=0.1487
d=2.877*10^(-2)
sd=0.002*10^(-2)
mmol=348.72*10^(-3)

untergrund2=read.table("../data/UntergrundNacht.txt",dec=",")[1:45,]

Uu=sum(untergrund2$V1)/length(untergrund2$V1)
nu=sum(untergrund2$V3)/length(untergrund2$V3)
tu=sum(untergrund2$V2)
snu=sqrt(nu/tu)

samarium2=read.table("../data/Samarium2.txt",dec=",")

U=sum(samarium2$V1)/length(samarium2$V1)
n=sum(samarium2$V3)/length(samarium2$V3)
t=sum(samarium2$V2)
sn=sqrt(n/t)

nges=n-nu
snges=sqrt(sn^2+snu^2)

Thalb=log(2)*Rrho*Na*hrel*pi*d^2/(8*nges*mmol)
ThalbJahre=Thalb/3600/24/365.25
sThalbJahre=ThalbJahre*sqrt(4*(sd/d)^2+(snges/nges)^2)

results=roundfunc(c(ThalbJahre,sThalbJahre)/10^11)

cat("T_(1/2) = (",results[1]," +- ",results[2],")*10^11 Jahre",sep="")