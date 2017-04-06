#####Figure X in paper
load("output/ODBA/odba_data_1sec.RData")

odba$id2<-paste0(odba$name,odba$date)
swims<-unique(odba$id2)

setEPS()
postscript("figs/Estimate_strokes.eps",width=7.5,height=7.5)

sample<-odba[odba$id==swims[24],]

par(mfrow = c(2,2),mar=c(2,4,1,0))
peak<-find_peaks(sample$DynODBA.xz,20)
plot(sample$DynODBA.xz,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
mtext("Dynamic acceleration of the x axis (g)",side=2,line=2)
axis(1,at = seq(0,2500,by=75), labels=seq(0,100,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.xz[peak])
abline(h=0)
text(1250,-0.9,"est. strokes = 35")
text(50,1, "A")

par(mar=c(2,2,1,1))
peak<-find_peaks(sample$DynODBA.x,30)
plot(sample$DynODBA.x,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
axis(1,at = seq(0,2500,by=75), labels=seq(0,100,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.x[peak])
abline(h=0)
text(1250,-0.9,"est. strokes = 28")
text(50,1, "B")

par(mar=c(4,4,0,0))
peak<-find_peaks(sample$DynODBA.x,40)
plot(sample$DynODBA.x,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
mtext("Dynamic acceleration of the x axis (g)",side=2,line=2)
mtext("Time (seconds)",side=1,line=2)
axis(1,at = seq(0,2500,by=75), labels=seq(0,100,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.x[peak])
text(1250,-0.9,"est. strokes = 25")
text(50,1, "C")

par(mar=c(4,2,0,1))
peak<-find_peaks(sample$DynODBA.x,50)
plot(sample$DynODBA.x,type="l",ylab="",xlab="",axes=F,ylim=c(-1,1))
mtext("Time (seconds)",side=1,line=2)
axis(1,at = seq(0,2500,by=75), labels=seq(0,100,by=3),pos=-1)
axis(2,pos=0, at=seq(-1,1,by=0.2))
points(peak,sample$DynODBA.x[peak])
text(1250,-0.9,"est. strokes = 24")
text(50,1, "D")

dev.off()



sample<-odba[odba$id==swims[24],]

shape <- diff(sign(diff(sample$DynODBA.x, na.pad = FALSE)))
pks <- sapply(which(shape < 0), FUN = function(i){
  z <- i - m + 1
  z <- ifelse(z > 0, z, 1)
  w <- i + m + 1
  w <- ifelse(w < length(x), w, length(x))
  if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
})
pks <- unlist(pks)
pks
