library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(sqldf)
library(chron)


crash <- read.csv("F:/caps/data/Crash-02-28.csv",sep=",", header=TRUE)
crashreport2 <- read.csv("F:/caps/data/CrashReports2.csv",sep=",", header=TRUE)
highwaycrash <- read.csv("F:/caps/data/HighwayCrash-1-2016.csv",sep=",", header=TRUE)
highway <- read.csv("F:/caps/data/Highway.csv",sep=",", header=TRUE)



#use highwaycrash & crash to merge into c
c <- sqldf("select ID, CrashDate, CrashTime, HighwayID, HWY_Route, HWY_Section,
           DayOfWeekUSA, DayName,Month, MonthName from crash") %>% 
    mutate(CrashDate=as.Date(CrashDate,"%m/%d/%Y"),
           CrashTime=chron(times=CrashTime),
           Year=year(CrashDate),
           Day=day(CrashDate)) %>% 
    left_join(sqldf("select HighwayKey as HighwayID, logmile from highwaycrash"), by=c("HighwayID"))



#counts of crashes by Year, Route
route_n <- sqldf("select HWY_Route, Year, count(*) as n from c 
                 group by Year, HWY_Route order by Year asc, n desc")



#use route 71 in 2015
c71 <- na.omit(subset(c, Year==2015 & HWY_Route %in% c(49))) %>% 
  mutate(Weekend = ifelse(DayOfWeekUSA==1 | DayOfWeekUSA==7, 1, 0),
         shift12 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="02:00:00"), 1, 
                   ifelse(CrashTime>=chron(times="02:00:00") & CrashTime<chron(times="04:00:00"), 2, 
                   ifelse(CrashTime>=chron(times="04:00:00") & CrashTime<chron(times="06:00:00"), 3, 
                   ifelse(CrashTime>=chron(times="06:00:00") & CrashTime<chron(times="08:00:00"), 4,
                   ifelse(CrashTime>=chron(times="08:00:00") & CrashTime<chron(times="10:00:00"), 5, 
                   ifelse(CrashTime>=chron(times="10:00:00") & CrashTime<chron(times="12:00:00"), 6, 
                   ifelse(CrashTime>=chron(times="12:00:00") & CrashTime<chron(times="14:00:00"), 7, 
                   ifelse(CrashTime>=chron(times="14:00:00") & CrashTime<chron(times="16:00:00"), 8,
                   ifelse(CrashTime>=chron(times="16:00:00") & CrashTime<chron(times="18:00:00"), 9, 
                   ifelse(CrashTime>=chron(times="18:00:00") & CrashTime<chron(times="20:00:00"), 10,
                   ifelse(CrashTime>=chron(times="20:00:00") & CrashTime<chron(times="22:00:00"), 11, 12))))))))))),
         shift6 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="04:00:00"), 1, 
                  ifelse(CrashTime>=chron(times="04:00:00") & CrashTime<chron(times="08:00:00"), 2, 
                  ifelse(CrashTime>=chron(times="08:00:00") & CrashTime<chron(times="12:00:00"), 3, 
                  ifelse(CrashTime>=chron(times="12:00:00") & CrashTime<chron(times="16:00:00"), 4,
                  ifelse(CrashTime>=chron(times="16:00:00") & CrashTime<chron(times="20:00:00"), 5, 6))))),
         shift3 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="08:00:00"), 1, 
                  ifelse(CrashTime>=chron(times="08:00:00") & CrashTime<chron(times="16:00:00"), 2, 3)),
         shift2 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="12:00:00"), 1, 2))

gplot <- ggplot(data = c71, mapping = aes(Logmile, alpha = 0.1))
gplot + geom_histogram(aes(y=..density..), bins = 50) + geom_density() + facet_grid(DayName~.)
gplot + geom_density(mapping = aes(col = MonthName))


#Week of Day vs. 2-hour Period (12 shifts)
c71 %>% 
  ggplot(mapping = aes(Logmile, alpha = 0.1)) + geom_histogram() + 
  geom_density(stat = "count") + facet_grid(shift12~.)

c71 %>% 
  ggplot(mapping = aes(CrashTime, alpha = 0.1)) + geom_histogram(bins = 24) + 
  geom_density(stat = "count") + facet_grid(DayName~.)

c71 %>% 
  ggplot(mapping = aes(CrashDate, alpha = 0.1)) + geom_histogram(bins = 50) + 
  geom_density(stat = "count") + facet_grid(shift12~.)

c71 %>% 
  ggplot(mapping = aes(CrashTime, CrashDate)) + geom_bin2d(bins = 50)
