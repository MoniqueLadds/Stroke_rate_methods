#####Make the plots
rm(list=ls())

#########density plot
load("data/tape/Zpredictions.RData")
source("r/tape/Functions.R")

##add animal info and actual number of strokes to the data frame
dat<-read.csv("data/tape/odba_vo2_data.csv")
predictions<-merge(predictions,dat[,c(2,18:23)],all.x = TRUE, by = "id2")
predictions<-predictions[!predictions$id2=="Rocky31-10-14_1.csv",]
predictions$name<-droplevels(predictions$name)


##find the differences
  names(predictions)
  
  stroke<-data.frame(predictions[c(1,27:32)], 
                     apply(predictions[2:26],2,diffs))
  
  stroke<-stroke[complete.cases(stroke),]


###Summarise the differences
summary.x<-apply(X = stroke[7:32],2,summary)
range.x<-apply(stroke[7:32],2,function(x) max(x)-min(x))
median.x<-apply(stroke[7:32],2,median)
mean.x<-apply(X = stroke[7:32],2,mean)
sd.x<-apply(X = stroke[7:32],2,sd)
ztest.x<-apply(X = stroke[7:32],2,function(x) z.test(a=x,mu=0,var=15))

apply(stroke[7:32],2,t.test)

export.x<-cbind(mean.z,sd.z,median.z,range.z,ztest.z)
write.csv(export.x,"output/peaks/summarystatsZ.csv")

###individual errors
indiv<-unique(as.character(stroke$name))

output<-NULL
predict<-NULL
for(l in 1:length(indiv)){
  
  #extract out individual
  dat<-stroke[stroke$name==indiv[l],]
  
  ##Find the mean difference
  indiv.mean<-apply(dat[8:32],2,mean)
  
  #find the corresponding RM and gradient
  best.name<-names(indiv.mean[abs(indiv.mean)==min(abs(indiv.mean))])
  best.mean<-min(abs(indiv.mean))
  
  #extract the best data
  best.diff<-dat[,names(dat)==best.name]
  
  #and the corresponding strokes
  strokes<-dat$stroke_rate
  
  #calculate the % diff
  percent.diff<-data.frame(cbind(best.diff,strokes))
  percent.diff$diff<-(percent.diff$best.diff+abs(percent.diff$strokes))/percent.diff$best.diff
  percent.diff$diff[which(percent.diff$diff==Inf)]<-0
  avg.diff<-mean(percent.diff$diff)
  
  output <- cbind(indiv[l],best.name,best.mean,avg.diff)
  
  predict<-data.frame(rbind(predict,output))
}


#make the histogram
tiff("figs/tape/tape_zhist.tiff",width = 600, height = 400)


#extract the data
plotData <- stroke$peak_0.4sec_30_dyn.z 
par(mar=c(3.5,3.5,1,1))
h<-hist(plotData, breaks=10,border="white", xlab="",ylab = "",main="",
        ylim=c(0,30),xlim=c(-30,50)) 
text(50,30,"B",cex=1.5)
mtext("Predicitons",1,2)
mtext("Frequency",2,2.2)
###function for the additional lines
density_lines<-function(lineData,col){
  xfit<-seq(min(lineData),max(lineData),length=40) 
  yfit<-dnorm(xfit,mean=mean(lineData),sd=sd(lineData)) 
  yfit <- yfit*10*length(lineData) 
  lines(xfit, yfit, col=col, lwd=2)
}

#lineData <- stroke$peak_0.4sec_10_dyn.z
#density_lines(lineData, "mediumorchid1")

lineData <- stroke$peak_0.4sec_20_dyn.z
density_lines(lineData, "hotpink3")

lineData <- stroke$peak_0.4sec_30_dyn.z
density_lines(lineData, "darkorchid1")

#lineData <- stroke$peak_0.4sec_40_dyn.z
#density_lines(lineData, "darkorchid1")

lineData <- stroke$peak_0.4sec_50_dyn.z
density_lines(lineData, "darkmagenta")

#lineData <- stroke$peak_1sec_10_dyn.z
#density_lines(lineData, "darkolivegreen")

lineData <- stroke$peak_1sec_20_dyn.z
density_lines(lineData, "chartreuse")

lineData <- stroke$peak_1sec_30_dyn.z
density_lines(lineData, "chartreuse3")

#lineData <- stroke$peak_1sec_40_dyn.z
#density_lines(lineData, "chartreuse4")

lineData <- stroke$peak_1sec_50_dyn.z
density_lines(lineData, "darkgreen")

#lineData <- stroke$peak_2sec_10_dyn.z
#density_lines(lineData, "cadetblue1")

lineData <- stroke$peak_2sec_20_dyn.z
density_lines(lineData, "deepskyblue")

lineData <- stroke$peak_2sec_30_dyn.z
density_lines(lineData, "blue")

#lineData <- stroke$peak_2sec_40_dyn.z
#density_lines(lineData, "cadetblue4")

lineData <- stroke$peak_2sec_50_dyn.z
density_lines(lineData, "blue4")

#lineData <- stroke$peak_3sec_10_dyn.z
#density_lines(lineData, "tan")

lineData <- stroke$peak_3sec_20_dyn.z
density_lines(lineData, "orange")

lineData <- stroke$peak_3sec_30_dyn.z
density_lines(lineData, "darkorange")

#lineData <- stroke$peak_3sec_40_dyn.z
#density_lines(lineData, "coral")

lineData <- stroke$peak_3sec_50_dyn.z
density_lines(lineData, "orangered3")

#lineData <- stroke$peak_4sec_10_dyn.z
#density_lines(lineData, "indianred")

lineData <- stroke$peak_4sec_20_dyn.z
density_lines(lineData, "firebrick1")

lineData <- stroke$peak_4sec_30_dyn.z
density_lines(lineData, "red")

#lineData <- stroke$peak_4sec_40_dyn.z
#density_lines(lineData, "red4")

lineData <- stroke$peak_4sec_50_dyn.z
density_lines(lineData, "violetred4")



legend(-32,30,lty=c(1,1),cex=0.8,bty="n",bg="n",
       c(#"RM:0.4; G:10",
         "RM:0.4; G:20",
         "RM:0.4; G:30",
         #"RM:0.4; G:40",
         "RM:0.4; G:50*",
         #"RM:1; G:10",
         "RM:1; G:20",
         "RM:1; G:30",
         #"RM:1; G:40",
         "RM:1; G:50*",
         #"RM:2; G:10",
         "RM:2; G:20",
         "RM:2; G:30",
         #"RM:2; G:40",
         "RM:2; G:50*",
         #"RM:3; G:10",
         "RM:3; G:20",
         "RM:3; G:30",
         #"RM:3; G:40",
         "RM:3; G:50*",
         #"RM:4; G:10",
         "RM:4; G:20",
         "RM:4; G:30",
         #"RM:4; G:40",
         "RM:4; G:50*"),
       col=c(#"mediumorchid1",
             "hotpink3",
             "darkorchid1",
             #"darkorchid2",
             "darkmagenta",
             #"darkolivegreen1",
             "chartreuse",
             "chartreuse3",
             #"chartreuse4",
             "darkgreen",
             #"cadetblue1",
             "deepskyblue",
             "blue",
             #"cadetblue4",
             "blue4",
             #"tan",
             "orange",
             "darkorange",
             #"coral",
             "darkorange4",
             #"indianred",
             "firebrick1",
             "red",
             #"red4",
             "violetred4"))

dev.off()
