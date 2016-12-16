import_clean<-function(filt_time,samples,threshold){
  library(signal)
  require(caTools)
  library(pracma)
  library(MESS)
  
  for (i in 1:length(names)){
    files<-list.files(paste0("data/swims_null/",names[i]))
    
    for (l in 1:length(files)){
      df<-read.csv(paste0("data/swims_null/",names[i],"/",files[l]),stringsAsFactors=FALSE)  
      cat(paste0(names[i],files[l]))
      x<-df[1,1]
      cat(x,"\n")
      
      df$time<-as.POSIXct(df$time,format="%d/%m/%Y %H:%M")
      
      start <- df[1,1]
      interval <- (60/32)/60
      
      end <- start + as.difftime((nrow(df)*interval), units="secs")
      
      times<-seq(from=start, by=interval, to=end)
      
      df$time<-times[1:nrow(df)]
      
      #keep raw data
      df<-df[,c(1:8,16:17)]       
      names(df)<-c("time","depth","temp","x","y","z","speed","divenum",
                   "start.time","end.time")
      
      ##filter for noise
      df$x<-hampel(df$x, k=filt_time)$y
      
      df$y<-hampel(df$y, k=filt_time)$y
      
      df$z<-hampel(df$z, k=filt_time)$y
      
      ###Use Hassans code to make new ODBA
      #result <- getAccDynODBA(df, "runmean", time)
      
      df$grav.x<-runmean(df$x,samples)
      df$grav.y<-runmean(df$y,samples)
      df$grav.z<-runmean(df$z,samples)  
      df$DynODBA.x<-df$x-df$grav.x
      df$DynODBA.y<-df$y-df$grav.y
      df$DynODBA.z<-df$z-df$grav.z 
      
      df$ODBA <- (abs(df$DynODBA.x)+abs(df$DynODBA.y)+abs(df$DynODBA.y))
      df$VeDBA <- sqrt((df$DynODBA.x)^2+(df$DynODBA.y)^2+(df$DynODBA.y)^2)
      df$PDBA <- (abs(df$DynODBA.x)+abs(df$DynODBA.y))
      
      ###Change the threshold
      df<-df[df$ODBA>threshold,]
      
      #df$odba<-abs(df$dx)+abs(df$dy)+abs(df$dz)
      odba.auc  <- auc(df$time, df$ODBA)
      
      #Make VeDBA
      #df$vedba<-sqrt(df$dx^2+df$dy^2+df$dz^2 ) 
      vedba.auc   <- auc(df$time, df$VeDBA)
      
      pdba.auc  <- auc(df$time, df$PDBA)
      
     
      #####make summary stats for dives
      dive.dat<-df[1,]
      dive.dat$mean.speed<-mean(df$speed,na.rm=TRUE)
      dive.dat$mean.temp<-mean(df$temp,na.rm=TRUE)
      dive.dat$date<-df$date[1]    
      dive.dat$ODBA.mean<-mean(df$ODBA,na.rm=TRUE)
      dive.dat$ODBA.auc<-odba.auc
      dive.dat$vedba.mean<-mean(df$VeDBA,na.rm=TRUE)
      dive.dat$vedba.auc<-vedba.auc
      dive.dat$PDBA.mean<-mean(df$PDBA,na.rm=TRUE)
      dive.dat$PDBA.auc<-pdba.auc
      dive.dat$name<-names[i]
      dive.dat$date<-files[l]
      
      #browser()
      df$name<-names[i]
      df$date<-files[l]
      
      #add summary dive to data file
      diving.dat<-rbind(diving.dat,dive.dat)
      
      #add full dive to data file
      dat<-rbind(dat,df)
            
    }
    
    save(dat,file=paste0("data/clean_data.RData"))
    save(diving.dat,file=paste0("data/clean_data_summary.RData"))
  }
  
}



add_strokes.x<-function(samples){
  
for(i in 1: length(swims)){
  sample<-dat[dat$divenum==swims[i],]
  peaks<-swims[i]
  for(l in 1:length(grd)){
    peak<-find_peaks(sample$DynODBA.x,grd[l])
    peaks<-cbind(peaks,length(peak))
  }
  output<-rbind(output,peaks)
}

output<-data.frame(output)
output$V2<-as.numeric(as.character(output$V2))
output$V3<-as.numeric(as.character(output$V3))
output$V4<-as.numeric(as.character(output$V4))
output$V5<-as.numeric(as.character(output$V5))
output$V6<-as.numeric(as.character(output$V6))
colnames(output)<-c("divenum",paste0("peak_",samples,"_60"),paste0("peak_",samples,"_70"),
                    paste0("peak_",samples,"_80"),paste0("peak_",samples,"_90"),paste0("peak_",samples,"_100"))
return(output)
}


add_strokes.z<-function(samples){
  
  for(i in 1: length(swims)){
    sample<-dat[dat$divenum==swims[i],]
    peaks<-swims[i]
    for(l in 1:length(grd)){
      peak<-find_peaks(sample$DynODBA.z,grd[l])
      peaks<-cbind(peaks,length(peak))
    }
    output<-rbind(output,peaks)
  }
  
  output<-data.frame(output)
  output$V2<-as.numeric(as.character(output$V2))
  output$V3<-as.numeric(as.character(output$V3))
  output$V4<-as.numeric(as.character(output$V4))
  output$V5<-as.numeric(as.character(output$V5))
  output$V6<-as.numeric(as.character(output$V6))
  colnames(output)<-c("divenum",paste0("peak_",samples,"_60"),paste0("peak_",samples,"_70"),
                      paste0("peak_",samples,"_80"),paste0("peak_",samples,"_90"),paste0("peak_",samples,"_100"))
  return(output)
}


add_strokes.xz<-function(samples){
  
  for(i in 1: length(swims)){
    sample<-dat[dat$divenum==swims[i],]
    peaks<-swims[i]
    for(l in 1:length(grd)){
      peak<-find_peaks(sample$DynODBA.xz,grd[l])
      peaks<-cbind(peaks,length(peak))
    }
    output<-rbind(output,peaks)
  }
  
  output<-data.frame(output)
  output$V2<-as.numeric(as.character(output$V2))
  output$V3<-as.numeric(as.character(output$V3))
  output$V4<-as.numeric(as.character(output$V4))
  output$V5<-as.numeric(as.character(output$V5))
  output$V6<-as.numeric(as.character(output$V6))
  colnames(output)<-c("divenum",paste0("peak_",samples,"_60"),paste0("peak_",samples,"_70"),
                      paste0("peak_",samples,"_80"),paste0("peak_",samples,"_90"),paste0("peak_",samples,"_100"))
  return(output)
}
