rm(list=ls())

source("r/Functions.R")

files<-list.files("data/tape/ODBA/")

#set the gradients to test for finding peaks
grad<-c(10,20,30,40,50)



#make stroke predictions for different running means and different gradients
for(k in 1: length(files)){
  
  load(paste0("data/tape/ODBA/",files[k]))
  
  ##make a variable that adds dyn.x and dyn.z together - Jeanniard
  odba$DynODBA.xz<-odba$DynODBA.x+odba$DynODBA.z
  
  odba$id2<-paste0(odba$name,odba$date)
  swims<-unique(odba$id2)
  
  output<-NULL
  for(j in 1:length(swims)){
    
    sample<-odba[odba$id==swims[j],]
    
    peaks<-swims[j]
    for(l in 1:length(grad)){
      peak<-find_peaks(sample$DynODBA.xz,grad[l])  ###x or z axis? ODBA or VeDBA?
      peaks<-cbind(peaks,length(peak))
    }
    
    output<-rbind(output,peaks)
  }
  
  save(output,file=paste0("data/tape/dyn.xz/","predict_",
                          substr(files[k],11,nchar(files[k]))))
}

##load the new data frame and merge it with the larger dataset

direct<-"output/peaks/dyn.xz/"    ###change for x or z
direct.files<-list.files(direct)

load(paste0(direct,direct.files[1]))
output<-data.frame(output)

###convert variables to numberic
output$V2<-as.numeric(as.character(output$V2))
output$V3<-as.numeric(as.character(output$V3))
output$V4<-as.numeric(as.character(output$V4))
output$V5<-as.numeric(as.character(output$V5))
output$V6<-as.numeric(as.character(output$V6))

#labels for data
file="dyn.xz"   #change based on variable
time=substr(strsplit(direct.files[1],"_")[[1]][2],1,
            nchar(strsplit(direct.files[1],"_")[[1]][2])-6)
colnames(output)<-c("id2",paste0("peak_",time,"_10_",file),
                    paste0("peak_",time,"_20_",file),
                    paste0("peak_",time,"_30_",file),
                    paste0("peak_",time,"_40_",file),
                    paste0("peak_",time,"_50_",file))

predictions<-output



for( i in 2:length(direct.files)){
  
  load(paste0(direct,direct.files[i]))
  output<-data.frame(output)
  
  ###convert variables to numberic
  output$V2<-as.numeric(as.character(output$V2))
  output$V3<-as.numeric(as.character(output$V3))
  output$V4<-as.numeric(as.character(output$V4))
  output$V5<-as.numeric(as.character(output$V5))
  output$V6<-as.numeric(as.character(output$V6))
  
  #labels for data
  file="dyn.xz"   #change based on variable
  time=substr(strsplit(direct.files[i],"_")[[1]][2],1,
              nchar(strsplit(direct.files[i],"_")[[1]][2])-6)
  colnames(output)<-c("id2",paste0("peak_",time,"_10_",file),
                      paste0("peak_",time,"_20_",file),
                      paste0("peak_",time,"_30_",file),
                      paste0("peak_",time,"_40_",file),
                      paste0("peak_",time,"_50_",file))
  
  ##make new data
  predictions<-merge(predictions,output,by="id2",all.x=TRUE)    
}


#save the large file
save(predictions,file="data/tape/XZpredictions.RData")


