#Question 1:------------

data(Titanic)
Titanic<-as.data.frame(Titanic)
survival_age<-aggregate(Freq~Age+Survived,data=Titanic,sum)
children_rate<-Survival_age$Freq[3]/(Survival_age$Freq[3]+Survival_age$Freq[1])
adult_rate<-Survival_age$Freq[4]/(Survival_age$Freq[2]+Survival_age$Freq[4])
survival_sex<-aggregate(Freq~Sex+Survived,data=Titanic,sum)
men_rate<-Survival_sex$Freq[3]/(Survival_sex$Freq[3]+Survival_sex$Freq[1])
women_rate<-Survival_sex$Freq[4]/(Survival_sex$Freq[2]+Survival_sex$Freq[4])

#As expected, children and women had higher survival rates than adults and Men. 

#Question 2:---------------
#declaring the function:
prismDat<-function(path){
  data.path<-path
  prism.files<-list.files(path)
  file.names<-sub("cv_","", prism.files)
  file.names<-sub("_daily.csv","", file.names)
  glob.path <- paste0(data.path, "*", ".csv")  # this is the englobing string that can find the files. 
  dataFiles <- lapply(Sys.glob(glob.path), read.csv, skip=11, head=F)
  climvars <- c("pdate", "ppt", "tmin", "tmean", "tmax")
  dataFiles <- lapply(dataFiles, setNames, climvars)
  names(dataFiles)<-file.names
  return(dataFiles)
}  
mydata<-prismDat("data/") # retrieving the data

temp_min<-function(df,threshold){ #function to check threshold and  write new dataframe
  for(i  in 1:nrow(df)){
    if(df$tmin[i]<threshold){
      result<-FALSE
    }else{
      result<-TRUE
    }#end else
    df$thresh[i]<-result  
  } #end for
  return(df)
}#end function

test<-lapply(mydata,temp_min,5) #testing function
head(test$grid1,2) #showing results

#Question 3:--------
library(rnoaa)
library(lubridate)
library(dplyr)
options(nooakey ="iczdfxvFGPnTLpqiOCvbjsIpOhTFMUTi")

capeGrimpTemp<-c()
for(i in 1985:2019){
  ini<-paste0(i,"-01-01")
  fin<-paste0(i,"-12-31")
  temp<-ncdc(datasetid='GSOM', stationid = 'GHCND:ASN00091245', datatypeid='TAVG', startdate = ini, enddate = fin, add_units=TRUE)
  capeGrimpTemp<-rbind(capeGrimpTemp,temp$data)
  }
kiwi<-capeGrimpTemp
kiwi$date<-as.Date(substr(kiwi$date,1,10))
kiwi_result<-round(aggregate(kiwi$value,by=list(year(kiwi$date)),mean),3)
kiwi_count<-aggregate(kiwi$date,by=list(year(kiwi$date)),length)
kiwi_result<-cbind(kiwi_result,kiwi_count$x)
names(kiwi_result)<-c("Year","Tavg","Mons")
head(kiwi_result)
