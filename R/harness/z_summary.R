#####Make the plots
rm(list=ls())

#########density plot
load("data/harness/Zpredictions.RData")
source("r/harness/Functions.R")

##find the differences
names(df)
stroke<-data.frame(df[c(1:19)], 
                   apply(df[20:44],2,diffs))
stroke<-stroke[complete.cases(stroke),]


##summarise the differences
summary.z<-apply(X = stroke[20:44],2,summary)
range.z<-apply(stroke[20:44],2,function(x) max(x)-min(x))
median.z<-apply(stroke[20:44],2,median)
mean.z<-apply(X = stroke[20:44],2,mean)
sd.z<-apply(X = stroke[20:44],2,sd)
ztest.z<-apply(X = stroke[20:44],2,function(x) z.test(a=x,mu=0,var=15))

#t.test(stroke$peak_32_60,mu=0)

export.z<-cbind(mean.z,sd.z,median.z,range.z,ztest.z)
write.csv(export.z,"output/harness/summarystatsZ.csv")


###individual errors
indiv<-unique(stroke$Animal)

#extract out individual
dat<-stroke[stroke$Animal==indiv[1],]

##Find the mean difference
indiv.mean<-apply(dat[20:44],2,mean)
#find the corresponding RM and gradient
best.name<-names(indiv.mean[abs(indiv.mean)==min(abs(indiv.mean))])
best.mean<-min(abs(indiv.mean))

#extract the best data
best.diff<-dat[,names(dat)==best.name]
#and the corresponding strokes
strokes<-dat$strokes
#calculate the % diff
percent.diff<-data.frame(cbind(best.diff,strokes))
percent.diff$diff<-(percent.diff$best.diff+abs(percent.diff$strokes))/percent.diff$best.diff
percent.diff$diff[which(percent.diff$diff==Inf)]<-0
avg.diff<-mean(percent.diff$diff)


tiff("figs/harness/zhist.tiff",width = 600, height = 400)

#make the histogram
#extract the data
plotData <- stroke$peak_13_60 

par(mar=c(4,4,1,1))
h<-hist(plotData, breaks=10,border="white", 
        xlab="", ylab = "", main="", cex.axis = 1.5,
        ylim=c(0,10),xlim=c(-30,30)) 
mtext("Frequency",2,2.2,cex = 1.5)
mtext("Predicitions",1,2, cex = 1.5)
text(30,10,"F",cex=1.5)


###function for the additional lines
density_lines<-function(lineData,col){
  xfit<-seq(min(lineData),max(lineData),length=40) 
  yfit<-dnorm(xfit,mean=mean(lineData),sd=sd(lineData)) 
  yfit <- yfit*diff(h$mids[1:2])*length(lineData) 
  lines(xfit, yfit, col=col, lwd=2)
}


lineData <- stroke$peak_13_60
density_lines(lineData, "mediumorchid1")

lineData <- stroke$peak_13_70
density_lines(lineData, "hotpink3")

lineData <- stroke$peak_13_80
density_lines(lineData, "darkorchid1")

lineData <- stroke$peak_13_90
density_lines(lineData, "darkorchid1")

lineData <- stroke$peak_13_100
density_lines(lineData, "darkmagenta")

lineData <- stroke$peak_32_60
density_lines(lineData, "darkolivegreen")

#lineData <- stroke$peak_32_70
#density_lines(lineData, "chartreuse")

lineData <- stroke$peak_32_80
density_lines(lineData, "chartreuse3")

lineData <- stroke$peak_32_90
density_lines(lineData, "chartreuse4")

#lineData <- stroke$peak_32_100
#density_lines(lineData, "darkgreen")

lineData <- stroke$peak_64_60
density_lines(lineData, "cadetblue1")

#lineData <- stroke$peak_64_70
#density_lines(lineData, "deepskyblue")

lineData <- stroke$peak_64_80
density_lines(lineData, "blue")

lineData <- stroke$peak_64_90
density_lines(lineData, "blue4")

#lineData <- stroke$peak_64_100
#density_lines(lineData, "cadetblue4")

lineData <- stroke$peak_96_60
density_lines(lineData, "tan")

#lineData <- stroke$peak_96_70
#density_lines(lineData, "orange")

lineData <- stroke$peak_96_80
density_lines(lineData, "orange")

lineData <- stroke$peak_96_90
density_lines(lineData, "orange4")

#lineData <- stroke$peak_96_100
#density_lines(lineData, "darkorange4")

lineData <- stroke$peak_128_60
density_lines(lineData, "indianred")

#lineData <- stroke$peak_128_70
#density_lines(lineData, "firebrick1")

lineData <- stroke$peak_128_80
density_lines(lineData, "red")

lineData <- stroke$peak_128_90
density_lines(lineData, "red4")

#lineData <- stroke$peak_128_100
#density_lines(lineData, "violetred4")



legend(-32,10.5,lty=c(1,1),cex=0.8,bty="n",bg="n",
       c("RM:0.4; G:60",
         #"RM:0.4; G:70",
         "RM:0.4; G:80",
         "RM:0.4; G:90",
         #"RM:0.4; G:100",
         "RM:1; G:60",
         #"RM:1; G:70",
         "RM:1; G:80",
         "RM:1; G:90",
         #"RM:1; G:100",
         "RM:2; G:60",
         #"RM:2; G:70",
         "RM:2; G:80",
         "RM:2; G:90",
         #"RM:2; G:100",
         "RM:3; G:60",
         #"RM:3; G:70",
         "RM:3; G:80",
         "RM:3; G:90",
         #"RM:3; G:100",
         "RM:4; G:60",
         #"RM:4; G:70",
         "RM:4; G:80",
         "RM:4; G:90"
         #"RM:4; G:100"
       ),
       col=c("mediumorchid1",
             #"hotpink3",
             "darkorchid1",
             "darkorchid2",
             #"darkmagenta",
             "darkolivegreen1",
             #"chartreuse",
             "chartreuse3",
             "chartreuse4",
             #"darkgreen",
             "cadetblue1",
             #"deepskyblue",
             "blue",
             "blue4",
             #"cadetblue4",
             "tan",
             #"orange",
             "orange",
             "orange4",
             #"darkorange4",
             "indianred",
             #"firebrick1",
             "red",
             "red4"
             #"violetred4"
       ))

dev.off()
