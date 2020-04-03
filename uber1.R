library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(DT)
library(scales)
library(lubridate)
#check  working directory and put all files in it
getwd()
#load data set
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")
#bind all data in new data frame 
uber.data<-rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
View(uber.data)
#convert hour minute date format
RequestDate <-format( as.POSIXct(strptime(uber.data$Date.Time,"%m/%d/%Y %H:%M",tz="")) ,format = "%m/%d/%Y")
class(RequestDate)
RequestHour <- format(as.POSIXct(strptime(uber.data$Date.Time,"%d/%m/%Y %H:%M",tz="")) ,format = "%H")
RequestHour
RequestMinute <- format(as.POSIXct(strptime(uber.data$Date.Time,"%d/%m/%Y %H:%M",tz="")) ,format = "%M")
RequestMinute
uber.data$RequestDate <- as.numeric(RequestDate)
uber.data$RequestHour <- as.numeric( RequestHour)
RequestDate
typeof(uber.data$RequestDate)
uber.data$RequestMinute<-RequestMinute
uber.data$Date.Time <- as.Date(uber.data$RequestDate, format = "%d/%m/%Y")
#create new feature dayslot 
for(i in 1:nrow(uber.data)) {
  if(uber.data[i,"uber.data$RequestHour"] <=4) {
    uber.data[i,"DaySlot"] <- "Early Morning"
  }else if (uber.data[i,"uber.data$RequestHour"] > 4 & uber.data[i,"RequestHour"] <=9){
    uber.data[i,"DaySlot"] <- "Morning"
  }else if (uber.data[i,"uber.data$RequestHour"] > 9 & uber.data[i,"RequestHour"] <=16){
    uber.data[i,"DaySlot"] <- "Mid Day"
  }else if (uber.data[i,"uber.data$RequestHour"] > 16 & uber.data[i,"RequestHour"] <=21){
    uber.data[i,"DaySlot"] <- "Evening"
  }else{
    uber.data[i,"DaySlot"] <- "Late Night"
  }}
#now concatenate 2 features in one for processs using paste () function
uber.data$loc<-paste(uber.data$Lat,uber.data$Lon,sep=' ')
#now use dplyr library which use in manipulating data and its grammer of manipulating.library easy to use function of aggregate (group,sum,count,and many more)
#grouping of single feature  according hour,days,weeks,months and other
install.packages("dplyr")
library(dplyr)
plottrips=
  function(data_var,col_name){
    # global datavar
    data_var <- data%>%
      group_by_(col_name)%>%
      dplyr::summarize(Total = n()) 
    data_var<<-data_var
  }
#1hourwise use 
hour_grp<-plottrips(hour_group,"uber.data$RequestHour")
hour_grp<-na.omit(hour_grp)
datatable(hour_grp)
#2location wise
loc_sum<-plottrips(loc_sum,"uber.data$loc")
loc_sum<-na.omit(loc_sum)
datatable(loc_sum)
#3 date wise
date_sum<-plottrip(date_sum,"uber.data$ReuestDate")
#3
#grouping more than one value (basewise and location wise)
base_loc_grp<-data%>%
  group_by(uber.data$Base,uber.data$loc)%>%
  dplyr::summarise(Total=n())
base_loc_grp<-na.omit(base_loc_grp)
datatable(base_loc_grp)
#2base-requestdate wise
base_Date_grp<-data%>%
  group_by(uber.data$Base,uber.data$RequestDate)%>%
  dplyr::summarise(Total=n())
base_Date_grp<-na.omit(base_Date_grp)
View(base_Date_grp)
#3base-requesthour wise
base_hour_grp<-data%>%
  group_by(uber.data$Base,uber.data$RequestHour)%>%
  dplyr::summarise(Total=n())
base_hour_grp<-na.omit(base_hour_grp)
View(base_hour_grp)
#4base-location-date group wise(3 feature aggregate)
base_loc_date_grp<-data%>%
  group_by(uber.data$Base,uber.data$loc,uber.data$RequestDate)%>%
  dplyr::summarise(Total=n())
base_loc_date_grp<-na.omit(base_loc_date_grp)
View(base_loc_date_grp)
#### now all other fetaures like  base,month,day,weekdays 
#### for this use old table which we create here
data<-rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
#format of date and time
#now using lubridate library convert string date time format which store in vector(day,month,date,hour,min and second ) convert in PSIXct object
data$Date.Time<- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
#use parse ymd_hms convert date and time in year month day and hour minute second 
#data$Date.Time<-ymd_hms(data$Date.Time)
#data$Date.Time
#factor used to categorize data in week days,month name, days and year
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label = TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label = TRUE))
View(data)
#1month wise
month_data<-plottrips(month_data,"month")
#use na.omit() omit is used to remove incomplete data from data frame
month_data<-na.omit(month_data)
View(month_data) # use to View dataframe 
#2 day_data
day_data<-plottrips(day_data,"day")
day_data<-na.omit(day_data)
#library DT is used in show datatable() and many other functions are in 
datatable(day_data)
#3week wise
week_data<-plottrips(week_data,"dayofweek")
week_dat<-na.omit(week_data)
View(week_data)
#4year wise
year_data<-plottrips(year_data,"year")
yera_data<-na.omit(year_data)
View(year_data)
#5basewise
base_Data<-plottrips(base_Data,"data$Base")
base_Data<-na.omit(base_Data)
View(base_Data)
#### now all data features are extract and manipulate now plot graph 
## for plotting a graph we use ggplot2 library which is best friend of data scientist to plot scientific graph.and ggthemes also use in.
#jiter graph
ggplot(day_data, aes(day,Total)) + geom_jitter()
#base and total
#qplot graph is used in ml and prediction of production. Here i plot base and total with data base_hour_grp and try to figure out each base with their hour use
qplot(base_hour_grp$`uber.data$Base`,base_hour_grp$Total, data=as.data.frame(base_hour_grp),colour=base_hour_grp$`uber.data$Base`) +
  ylab("Total") + xlab("Base")
#date base and total
qplot(base_Date_grp$`uber.data$RequestDate`,base_Date_grp$Total, data=as.data.frame(base_Date_grp),colour=base_Date_grp$`uber.data$Base`,xlim = c(20,31)) +
  ylab("Total") + xlab("Date")



