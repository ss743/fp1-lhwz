oldw <- getOption("warn")
options(warn = -1)

par(mar=c(5,5,2,2))

untergrund=read.table("../data/Untergrund.txt",dec=",")

x=untergrund$V1
t=untergrund$V2
y=untergrund$V3
sy=sqrt(y/t)
lowlimit=(y-sy)*(y>sy)

plotCI(x,y,ui=y+sy,li=lowlimit,err="y",sfrac=0.005,cex=0.6,pch=4,bty="l",xlab="U / V",ylab=expression(n / s^-1))
grid()

u=c(0,0,0,0,0,0,0,0,0,y)
su=c(0,0,0,0,0,0,0,0,0,sy)

uran=read.table("../data/uran2.txt",dec=",")

x=uran$V1
y=uran$V3
t=uran$V2
sy=sqrt(y/t)
y_corr=y-u
sy_corr=sqrt(sy^2+su^2)

plotCI(x,y,ui=sy+y,li=(y-sy)*(y>sy)+0.0001*(y<=sy),cex=0.6,pch=4,bty="l",log="y",ylim=c(10^-2,10^3),xlab="U / V",ylab=expression(n / s^-1))
#plotCI(x,y_corr,ui=sy_corr+y_corr,li=(y_corr-sy_corr*(y_corr>sy_corr)+0.0001*(y_corr<=sy_corr)),cex=0.6,pch=4,bty="l",log="y",ylim=c(10^-2,10^3),xlab="U / V",ylab="n / Hz",add=TRUE,col="green")
grid()


cat("Samarium:\n")
source("samarium.R")
cat("\n\nKalium:\n")
source("kalium.R")

options(warn = oldw)