library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(sqldf)
library(chron)
library(forecast)
library(bda)
library(kolmim)



crash <- read.csv("F:/caps/data/Crash-02-28.csv",sep=",", header=TRUE)
crashreport2 <- read.csv("F:/caps/data/CrashReports2.csv",sep=",", header=TRUE)
highwaycrash <- read.csv("F:/caps/data/HighwayCrash-1-2016.csv",sep=",", header=TRUE)
highway <- read.csv("F:/caps/data/Highway.csv",sep=",", header=TRUE)



#use highwaycrash & crash to merge into c
c <- crash %>% 
    mutate(CrashDate=as.Date(CrashDate,"%m/%d/%Y"),
           CrashTime=chron(times=CrashTime),
           Year=year(CrashDate),
           Day=day(CrashDate),
           Weekend = ifelse(DayOfWeekUSA %in% c(1, 2, 3, 4, 5), 1, 2),
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
           shift2 = ifelse(CrashTime>=chron(times="00:00:00") & CrashTime<chron(times="12:00:00"), 1, 2)) %>% 
    left_join(sqldf("select HighwayKey as HighwayID, Route as HWY_Route, logmile, 
                    Longitude as H_longitude, Latitude as H_latitude, AvgDailyTr, 
                    YearADT, TypeRoad, RouteSign, APHN, Access, TypeOperat, MedianType, 
                    NumberLane, roadid from highwaycrash"), by=c("HighwayID", "HWY_Route"))



#counts of crashes by Year, Route
route_n <- sqldf("select HWY_Route, Year, count(*) as n from c 
                 group by Year, HWY_Route order by Year asc, n desc")



#use route 71 in 2015
route <- function(year, num)
{
  mydata <- subset(c, Year==year & HWY_Route %in% num)
  return(mydata)
}

c71 <- route(2015, 71)
c49 <- route(2015, 49)
c30 <- route(2015, 30)

#Route 71: Logmile vs. Time = DayOfWeek
c71 %>% 
  ggplot(mapping = aes(Logmile, alpha = 0.1)) + 
  geom_density(aes(y = ..count.., col = factor(Weekend)))


#loess for crashes during weekdays then predict weekend
pred.crash <- function(data, nclass)
{
  #binning Logmile into nclass for Weekend == 1 and Weekend == 2
  out1 <- binning(x = data[which(data$Weekend==1),]$Logmile,
                  breaks = seq(0, max(data[which(data$Weekend==1),]$Logmile), 
                               max(data[which(data$Weekend==1),]$Logmile)/nclass))
  out2 <- binning(x = data[which(data$Weekend==2),]$Logmile,
                  breaks = seq(0, max(data[which(data$Weekend==2),]$Logmile),
                               max(data[which(data$Weekend==2),]$Logmile)/nclass))
  
  #fit loess on Weekend == 1
  mod <- loess(out1$counts~out1$mids, span = 0.1)
  windows()
  par(mfrow=c(2,2), family = "serif", cex = 1.2)
  
  #plot for weekend ==1 & 2 after binning
  plot(out1$counts~out1$mids, ylab = "Counts", xlab = "Logmile",
       xlim = c(0,max(data$Logmile)), ylim = c(0, max(out1$counts, out2$counts, na.rm = TRUE)))
  lines(out1$mids, predict(mod))
  points(out2$counts~out2$mids, col = 2)
  
  #calculate |obs. - pred.| as measurement
  abs.diff1 <- abs(predict(mod)-out1$counts)
  abs.diff2 <- abs(predict(mod, newdata = out2$mids)-out2$counts)
  
  #plot for cumsum|obs. - pred.| in order
  plot(1:nclass, cumsum(abs.diff1[order(-abs.diff1)]), type = "l", 
       ylim = c(min(abs.diff1, abs.diff2, na.rm = TRUE), max(sum(abs.diff1, na.rm = TRUE), sum(abs.diff2, na.rm = TRUE))),
       xlab = "size", ylab = "cumulative sum of |pred. - obs.|")
  lines(1:nclass, cumsum(abs.diff2[order(-abs.diff2)]), col = 2)
  
  #plot for Logmile vs. |obs. - pred.|
  plot(out1$mids, abs.diff1, type = "l",
       ylim = c(min(abs.diff, abs.diff2, na.rm = TRUE), max(abs.diff, abs.diff2, na.rm = TRUE)),
       xlab = "Logmile", ylab = "|pred. - obs.|")
  lines(out2$mids, abs.diff2, col = 2)
}
pred.crash(c71, 100)
pred.crash(c49, 100)
pred.crash(c30, 100)



#Route 71: Logmile vs. CrashTime
c71 %>% 
  ggplot(mapping = aes(Logmile, CrashTime)) + geom_bin2d(bins = 80)
c71 %>% 
  ggplot(mapping = aes(Logmile)) + geom_density(aes(y = ..count.., col = factor(shift12)))


#loess for crashes during selected variable then predict selected variable
pred.crash2 <- function(data, var, nclass)
{
  val <- NULL
  n <- n_distinct(data[var])
  for (i in 1:n)
  {
    assign(paste("out", i, sep = ""), 
           binning(x=data[which(data[var]==i),]$Logmile,
                   breaks = seq(0, max(data[which(data[var]==i),]$Logmile),
                                max(data[which(data[var]==i),]$Logmile/nclass))))
    val <- c(val, get(paste("out", i, sep = ""))$counts)
  }
  
  #fit loess on selected variable (i = 1)
  mod <- loess(out1$counts~out1$mids)

  #plot for selected variable by groups after binning  
  windows()
  par(mfrow=c(2, 2), family = "serif", cex = 1.2)
  plot(predict(mod, newdata = out1$mids)~out1$mids, ylab = "Counts", xlab = "Logmile", type = "l",
       xlim = c(0, max(data$Logmile)), ylim = c(0, max(val)))
  lines(out1$counts~out1$mids, lty = 2)
  for (i in 2:n)
  {
    lines(get(paste("out", i, sep = ""))$counts~get(paste("out", i, sep = ""))$mids, col = i, lty = 2)
  }
  
  #calculate |obs. - pred.| as measurement
  abs.diff <- abs(predict(mod)-out1$counts)
  for (i in 2:n)
  {
    abs.diff <- cbind(abs.diff, 
                      abs(predict(mod, newdata = get(paste("out", i, sep = ""))$mids)-
                            get(paste("out", i, sep = ""))$counts))
  }
  
  #plot for cumsum|obs. - pred.| in order
  plot(1:nclass, cumsum(abs.diff[, 1][order(-abs.diff[, 1])]), type = "l", 
       ylim = c(min(abs.diff, na.rm = TRUE), max(apply(abs.diff, 2, FUN = sum, na.rm = TRUE))),
       xlab = "size", ylab = "cumulative sum of |pred. - obs.|")
  for (i in 2:n)
  {
    lines(1:nclass, cumsum(abs.diff[, i][order(-abs.diff[, i])]), col = i)
  }
  
  #plot for Logmile vs. |obs. - pred.|
  plot(out1$mids, abs.diff[,1], type = "l",
       ylim = c(min(abs.diff, na.rm = TRUE), max(abs.diff, na.rm = TRUE)),
       xlab = "Logmile", ylab = "|pred. - obs.|")
  for (i in 2:n)
  {
    lines(get(paste("out", i, sep = ""))$mids, abs.diff[,i], col = i) 
  }
}
pred.crash2(c71, "shift6", 30)
pred.crash2(c71, "Weekend", 50)
pred.crash2(c71, "DayOfWeekUSA", 50)


#test of variances & means for Weekend==1, 0
test <- c71 %>% select(CrashDate) %>% group_by(CrashDate) %>% 
  count() %>% mutate(Weekend = ifelse(day.of.week(month(CrashDate), day(CrashDate), year(CrashDate)) %in% 
                                        c(1, 2, 3, 4, 5), 1, 2))
test %>% ggplot(aes(n)) + geom_density(aes(y=..count.., col = factor(Weekend)))

#Kolmogorov-Smirnov test
test1 <- ks.test(test[test$Weekend==1,]$n,test[test$Weekend==2,]$n)

#empirical CDF
a <- test[test$Weekend==1,]$n
b <- test[test$Weekend==2,]$n
ecdf.x <- ecdf(a)
ecdf.y <- ecdf(b)
plot(ecdf.x, xlim=c(min(c(a,b)), max(c(a,b))), main = "Empirical CDF")
plot(ecdf.y, add=TRUE, col = 2)
summary(a)
summary(b)

#Route 71: crashes over time
c71 %>% 
  ggplot(mapping = aes(CrashDate, alpha = 0.1)) + geom_histogram(bins = 50) + 
  geom_density(stat = "count") + facet_grid(shift12~.)
c71 %>% 
  ggplot(mapping = aes(CrashTime, alpha = 0.1)) + geom_histogram(bins = 50) + 
  geom_density(stat = "count")
c71 %>% 
  ggplot(mapping = aes(CrashTime, CrashDate)) + geom_bin2d(bins = 60)




data.frame(d(test_sub)) %>% ggplot(mapping = aes(index, diff)) + geom_line()
cpl <- d(holdout)
f7 <- ts(data.frame(d(test_sub))$diff, frequency = 1)
tsdisplay(f7)
plot(f7)
mod <- auto.arima(f7)
plot(forecast(mod, h= 100))

