#####Figure X in paper
source("R/harness/functions.R")

load("data/harness/samples/clean_data_64.RData")
dat$DynODBA.xz<-dat$DynODBA.x+dat$DynODBA.z

swims<-unique(dat$divenum)

#setEPS()
#postscript("figs/Estimate_strokes.eps",width=7.5,height=7.5)

tiff(filename = "figs/harness/example_plotXZ.tiff", width = 980, height = 520)

sample<-dat[dat$divenum==swims[13],] #actual strokes 32

par(mfrow = c(2,2),mar=c(2,4,1,0))
peak<-find_peaks(sample$DynODBA.xz,70)
plot(sample$DynODBA.xz,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
mtext("Dynamic acceleration of the x axis (g)",side=2,line=2)
axis(1,at = seq(0,5000,by=100), labels=seq(0,150,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.xz[peak])
abline(h=0)
text(1250,-0.9,paste0("est. strokes = ", length(peak)))
text(100,1, "A")

par(mar=c(2,2,1,1))
peak<-find_peaks(sample$DynODBA.xz,80)
plot(sample$DynODBA.xz,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
axis(1,at = seq(0,5000,by=100), labels=seq(0,150,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.xz[peak])
abline(h=0)
text(1250,-0.9,paste0("est. strokes = ", length(peak)))
text(100,1, "B")

par(mar=c(4,4,0,0))
peak<-find_peaks(sample$DynODBA.xz,90)
plot(sample$DynODBA.xz,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
mtext("Dynamic acceleration of the x axis (g)",side=2,line=2)
mtext("Time (seconds)",side=1,line=2)
axis(1,at = seq(0,5000,by=100), labels=seq(0,150,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.xz[peak])
text(1250,-0.9,paste0("est. strokes = ", length(peak)))
text(100,1, "C")

par(mar=c(4,2,0,1))
peak<-find_peaks(sample$DynODBA.xz,100)
plot(sample$DynODBA.xz,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
mtext("Time (seconds)",side=1,line=2)
axis(1,at = seq(0,5000,by=100), labels=seq(0,150,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.xz[peak])
text(1250,-0.9,paste0("est. strokes = ", length(peak)))
text(100,1, "D")

dev.off()