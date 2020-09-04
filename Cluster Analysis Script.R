library(ggplot2)
library(splitr)
library(dplyr)
library(lubridate)
library(tibbletime)
library(openair)




Traj2018 <-
  hysplit_trajectory(
    lat = 44.366,
    lon = -73.902,
    height = 976,
    duration = 144,
    direction = "backward",
    met_type =  "narr",
    days = seq(
      lubridate::ymd("2018-06-01"),
      lubridate::ymd("2018-09-30"),
      by = "day"),
    
    daily_hours = c(11,23)
  )

Traj2018<-filter(Traj2018, !is.na(hour_along))

Traj2019 <-
  hysplit_trajectory(
    lat = 44.366,
    lon = -73.902,
    height = 976,
    duration = 144,
    direction = "backward",
    met_type =  "narr",
    days = seq(
      lubridate::ymd("2019-06-01"),
      lubridate::ymd("2019-09-30"),
      by = "day"),
    
    daily_hours = c(11,23)
  )
Traj2019<-filter(Traj2019, !is.na(hour_along))

AWITraj<-rbind(Traj2018,Traj2019)


CloudDates<-select(WFCOA, date)

FullClust<-AWITraj%>%
  mutate(date = traj_dt_i - hours(5))%>%
  mutate(hour.inc = hour_along)

FullClust<as.data.frame(FullClust)
FullClust$date<-as.POSIXct(FullClust$date, format = "%Y-%m-%d %H:%M:%S")
trajCluster(FullClust, method = "Angle", n.cluster = 6)

Clust2018



AWIClust<-as.data.frame(AWIClust)
AWIClust$date<-as.POSIXct(AWIClust$date, format = "%Y-%m-%d %H:%M:%S")



AWIClust<-AWITraj%>%
  mutate(date = traj_dt_i - hours(5))%>%
  mutate(hour.inc = hour_along)
AWIClust<-as.data.frame(AWIClust)
AWIClust$date<-as.POSIXct(AWIClust$date, format = "%Y-%m-%d %H:%M:%S")

CloudDates$date<-as.factor(CloudDates$date)
AWIClust$date<-as.factor(AWIClust$date)
AWIClust<-right_join(AWIClust, CloudDates ,by = "date")
AWIClust<-filter(AWIClust, !is.na(height_i))
AWIClust$date<-as.POSIXct(AWIClust$date, format = "%Y-%m-%d %H:%M:%S")

AWIClust<-AWIClust%>%
  mutate(date = date + hours(5))





trajLevel(AWIClust$data, statistic = "frequency")


AWICluster<-trajCluster(AWIClust, n.cluster = 6, plot = TRUE, 
                        map.cols = openColours("Paired", 10),
                        method= "Euclid")




Clust18<-Traj2018%>%
mutate(date = traj_dt_i)%>%
  mutate(hour.inc = hour_along)
Clust18<-as.data.frame(Clust18)
Clust18$date<-as.POSIXct(Clust18$date, format = "%Y-%m-%d %H:%M:%S")
Clust2018<-trajCluster(Clust18, n.cluster = 6, plot = TRUE, type = "month" , method = "Angle")

Clust19<-Traj2019%>%
  mutate(date = traj_dt_i)%>%
  mutate(hour.inc = hour_along)
Clust19<-as.data.frame(Clust19)
Clust19$date<-as.POSIXct(Clust19$date, format = "%Y-%m-%d %H:%M:%S")
Clust2019<-trajCluster(Clust19, n.cluster = 6, plot = TRUE, type = "month", method = "Angle")



ClustData<-AWICluster$data
ClustData%>%
  group_by(cluster, hour_along)%>%
  dplyr::summarise(Height = mean(height, na.rm = TRUE))%>%
  ggplot()+
  geom_point(aes(x = hour_along, y = Height, color = as.factor(cluster)))


ggplot(ClustData)+
  geom_point(aes(x = hour_along, y = height, color = as.factor(cluster)))

WFC2018<-read.csv("WFC2018OAReanalyzed.csv", header = TRUE, sep = ",")
WFC2018$date<-as.POSIXct(paste(WFC2018$date), format = "%m/%d/%Y %H:%M")
WFC2018<-as_tbl_time(WFC2018, index = date)

WFC2019<-read.csv("WFC2019csv.csv", header = TRUE, sep = ",")
WFC2019$date<-as.POSIXct(paste(WFC2019$date), format = "%m/%d/%Y %H:%M")
WFC2019<-as_tbl_time(WFC2019, index = date)




ggplot(WFCOA)+
  geom_point(aes(x = date, y = Oxalate/SO4, color = LABPH))+
  scale_color_gradientn(colors = rainbow(6))

ClustData<-ClustData%>%
  mutate(date = traj_dt_i - hours(5))%>%
  dplyr::select(c(date, cluster))

  
ClustData<-unique(ClustData)
ClustData$date<-as.factor(ClustData$date)
WFCOA$date<-as.factor(WFCOA$date)
WFCOA<-rbind.fill(WFC2018,WFC2019)
WFCOA$date<-as.factor(WFCOA$date)
  


trajLevel(subset(AWICluster$data,lat > 30 & lat < 60 & lon >-150 & lon < 70), 
          type = "cluster", col = "increment", border = NA)

ClustandCloud<-left_join(WFCOA,ClustData, by = "date")


ClustandCloud$date<-as.POSIXct(ClustandCloud$date, format = "%Y-%m-%d %H:%M:%S")




ClustandCloud%>%
  group_by(cluster)%>%
  dplyr::summarise(SO4rat = mean(Oxalate/(CA), na.r))





ClustandCloud$LogWSOC<-with(ClustandCloud, log10(WSOC))

AOVTest<-aov(log10(Formate)~cluster,data = ClustandCloud)
TukeyHSD(AOVTest, conf.level = .9)

ggplot(ClustandCloud)+
  geom_point(aes(x = , y = Acetate, color = cluster))+
  facet_wrap(~cluster)



ggplot(ClustandCloud)+
  geom_density(aes(x = SO4 ,color = cluster))+
  facet_wrap(~cluster)+scale_x_log10()


ClustandCloud<-filter(ClustandCloud, !is.na(Month))

ClustandCloud%>%
  group_by(cluster)%>%
  dplyr::summarise(across(where(is.numeric)),~ mean(.x, na.rm = TRUE))







