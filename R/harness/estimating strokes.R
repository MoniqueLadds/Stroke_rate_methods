rm(list=ls())

library(signal)
library(chron)
library(lubridate)
library(MESS)
library(pracma)
require(caTools)

source("R/harness/functions.R")
source("R/harness/import_clean_swims.R")

####make new data file with clean data for prediction
names<-list.files("data/harness/swims_null/")
df<-NULL
grad<-c(10,20,30,40,50)
sample_hz<-c(13,32,64,96,128)


###combining peaks (stroke rate data)
vo2_data<-read.csv("data/harness/steller_vo2_data.csv",stringsAsFactors=FALSE)
vo2_data<-vo2_data[!vo2_data$divenum=="",]

grd<-c(60,70,80,90,100)

# c(13,32,64,96,128))
  load("data/harness/samples/clean_data_13.RData")
  swims<-unique(dat$divenum)
  dat$DynODBA.xz<-dat$DynODBA.x+dat$DynODBA.z
  output<-NULL
  output <- add_strokes.xz(13)
  df <- merge(vo2_data, output, by = "divenum", all.y = TRUE)
  
  load("data/harness/samples/clean_data_32.RData")
  swims<-unique(dat$divenum)
  dat$DynODBA.xz<-dat$DynODBA.x+dat$DynODBA.z
  output<-NULL
  output <- add_strokes.xz(32)
  df <- merge(df, output, by = "divenum", all.y = TRUE)
  
  load("data/harness/samples/clean_data_64.RData")
  swims<-unique(dat$divenum)
  dat$DynODBA.xz<-dat$DynODBA.x+dat$DynODBA.z
  output<-NULL
  output <- add_strokes.xz(64)
  df <- merge(df, output, by = "divenum", all.y = TRUE)
              
  load("data/harness/samples/clean_data_96.RData")
  swims<-unique(dat$divenum)
  dat$DynODBA.xz<-dat$DynODBA.x+dat$DynODBA.z
  output<-NULL
  output <- add_strokes.xz(96)
  df <- merge(df, output, by = "divenum", all.y = TRUE)
  
  load("data/samples/clean_data_128.RData")
  swims<-unique(dat$divenum)
  dat$DynODBA.xz<-dat$DynODBA.x+dat$DynODBA.z
  output<-NULL
  output <- add_strokes.xz(128)
  df <- merge(df, output, by = "divenum", all.y = TRUE)

save(df,file="output/XZpredictions.RData")
