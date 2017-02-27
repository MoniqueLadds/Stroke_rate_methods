#####Make the plots
rm(list=ls())

#########density plot
load("data/harness/Xpredictions.RData")
source("r/harness/Functions.R")

##find the differences
names(df)
stroke<-data.frame(df[c(1:19)], 
                   apply(df[20:44],2,diffs))
stroke<-stroke[complete.cases(stroke),]

##calculate "actual" amount of energy expended over a dive
df$energy_actual <- df$strokes*3.8

df$energy_96_60<-df$peak_96_60*3.8
df$energy_96_70<-df$peak_96_70*3.8
df$energy_96_80<-df$peak_96_80*3.8
df$energy_96_90<-df$peak_96_90*3.8
df$energy_96_100<-df$peak_96_100*3.8

df$energy.diff_60<-df$energy_actual-df$energy_96_60
df$energy.diff_70<-df$energy_actual-df$energy_96_70
df$energy.diff_80<-df$energy_actual-df$energy_96_80
df$energy.diff_90<-df$energy_actual-df$energy_96_90
df$energy.diff_100<-df$energy_actual-df$energy_96_100
hist(df$energy.diff_60)

tiff("figs/harness/energy_dist_harness.tiff", width = 140, height = 90, units = 'mm',
     res = 300, compression = 'lzw')

par(mfrow = c(2,2),mar = c(1,3.5,1,1))
hist(df$energy.diff_60, main="", xlab = "", ylab = "",
     axes = FALSE, ylim = c(0,10),xlim = c(-80,50))
axis(2, line = -0.45,cex.axis = 1, mgp = c(1,0.5,0))
mtext("Frequency", 2, 1.5)
text(40, 10, "A")
par(mar = c(1,2.5,1,1))
hist(df$energy.diff_70, main="", xlab = "", ylab = "",
     axes = FALSE,ylim = c(0,10),xlim = c(-80,50))
axis(2, line = -0.45, cex.axis = 1, mgp = c(1,0.5,0))
text(40, 10, "B")
par(mar = c(2.5,3.5,0,1))
hist(df$energy.diff_80, main="", xlab = "", ylab = "",
     ylim = c(0,10),xlim = c(-80,50), axes = FALSE)
axis(1, line = -0.3, mgp = c(1,0.5,0))
axis(2, line = -0.45, mgp = c(1,0.5,0))
mtext("Frequency", 2, 1.5)
mtext("Energy (J/kg)",1,1.5)
text(40, 10, "C")
par(mar = c(2.5,2.5,0,1))
hist(df$energy.diff_90, main="", xlab = "", ylab = "",
     ylim = c(0,10),xlim = c(-80,50), axes = FALSE)
axis(1, line = -0.3, mgp = c(1,0.5,0))
axis(2, line = -0.45)
mtext("Energy (J/kg)",1,1.5)
text(40, 10, "D")

dev.off()

#####Make the plots for the tape group
rm(list=ls())

#########tape data
load("data/tape/Xpredictions.RData")
source("r/tape/Functions.R")

##add animal info and actual number of strokes to the data frame
dat<-read.csv("data/tape/odba_vo2_data.csv")
predictions<-merge(predictions,dat[,c(2,18:23)],all.x = TRUE, by = "id2")
predictions<-predictions[!predictions$id2=="Rocky31-10-14_1.csv",]
predictions<-predictions[complete.cases(predictions$stroke_rate),]
predictions$name<-droplevels(predictions$name)


##find the differences
names(predictions)
stroke<-data.frame(predictions[c(1,27:32)], 
                   apply(predictions[2:26],2,diffs))
stroke<-stroke[complete.cases(stroke),]

##calculate "actual" amount of energy expended over a dive
predictions$energy_actual <- predictions$stroke_rate*3.8

predictions$stroke_freq <- predictions$stroke_rate/predictions$swim.secs

#calculate predicted energy expended
predictions$energy_3sec_10 <- predictions$peak_3sec_10_dyn.x*3.8
predictions$energy_3sec_20 <- predictions$peak_3sec_20_dyn.x*3.8
predictions$energy_3sec_30 <- predictions$peak_3sec_30_dyn.x*3.8
predictions$energy_3sec_40 <- predictions$peak_3sec_40_dyn.x*3.8
predictions$energy_3sec_50 <- predictions$peak_3sec_50_dyn.x*3.8

predictions$energy_diff_10 <- predictions$energy_actual - predictions$energy_3sec_10
predictions$energy_diff_20 <- predictions$energy_actual - predictions$energy_3sec_20
predictions$energy_diff_30 <- predictions$energy_actual - predictions$energy_3sec_30
predictions$energy_diff_40 <- predictions$energy_actual - predictions$energy_3sec_40
predictions$energy_diff_50 <- predictions$energy_actual - predictions$energy_3sec_50


h.plot<-hist(predictions$energy_diff_50)


tiff("figs/tape/energy_dist_tape.tiff", width = 140, height = 90, units = 'mm',
     res = 300, compression = 'lzw')

par(mfrow = c(2,2),mar = c(1,3.5,1,1))
hist(predictions$energy_diff_20, main="", xlab = "", ylab = "",
     axes = FALSE, ylim = c(0,55),xlim = c(-350,100))
axis(2, line = -0.45,cex.axis = 1, mgp = c(1,0.5,0))
mtext("Frequency", 2, 1.5)
text(100, 50, "A")
par(mar = c(1,2.5,1,1))
hist(predictions$energy_diff_30, main="", xlab = "", ylab = "",
     axes = FALSE, ylim = c(0,55),xlim = c(-350,100))
axis(2, line = -0.45, cex.axis = 1, mgp = c(1,0.5,0))
text(100, 50, "B")
par(mar = c(2.5,3.5,0,1))
hist(predictions$energy_diff_40, main="", xlab = "", ylab = "",
     xlim = c(-350,100), ylim = c(0,55), axes = FALSE)
axis(1, line = -0.2, mgp = c(1,0.5,0))
axis(2, line = -0.45, mgp = c(1,0.5,0))
mtext("Frequency", 2, 1.5)
mtext("Energy (J/kg)",1,1.5)
text(100, 50, "C")
par(mar = c(2.5,2.5,0,1))
hist(predictions$energy_diff_50, main="", xlab = "", ylab = "",
     xlim = c(-350,100), ylim = c(0,55), axes = FALSE)
axis(1, line = -0.2, mgp = c(1,0.5,0))
axis(2, line = -0.45)
mtext("Energy (J/kg)",1,1.5)
text(100, 50, "D")

dev.off()