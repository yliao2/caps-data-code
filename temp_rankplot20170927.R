library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(sqldf)
library(chron)
library(forecast)
library(bda)
library(kolmim)
library(np)
library(lattice)
library(data.table)



crash <- read.csv("F:/caps/data/Crash-02-28.csv",sep=",", header=TRUE)
crashreport2 <- read.csv("F:/caps/data/CrashReports2.csv",sep=",", header=TRUE)
highwaycrash <- read.csv("F:/caps/data/HighwayCrash-1-2016.csv",sep=",", header=TRUE)
highway <- read.csv("F:/caps/data/Highway.csv",sep=",", header=TRUE)



#use highwaycrash & crash to merge into c
c <- crash %>% select(CrashDate, CrashTime, HighwayID, HWY_Route, DayOfWeekUSA,
                      DayName, Month, MonthName) %>% 
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
  left_join(sqldf("select HighwayKey as HighwayID, Route as HWY_Route, 
                  logmile from highwaycrash"), by=c("HighwayID", "HWY_Route"))



#produce original counts and predictions|"t", 
#rank(true counts)|"t", rank(predictions)|"t"
#===============================================
crash.results <- function(year, route, t)
{
  #logmile from highway on "route" in "year"
  #-------------------------------------------
  assign(paste("c",route,"_bins"), highwaycrash %>% filter(Route==route) %>% select(Logmile) %>% 
           arrange_all() %>% distinct(Logmile))
  
  #merge crash("route","year"|"t")
  #------------------------------------------
  l <- unique(c[[t]])
  out <- NULL
  for (i in 1:length(l))
  {
    temp <- c %>% filter(Year==year & HWY_Route==route & get(t)==l[i]) %>% select(Logmile) %>% 
      group_by_all() %>% count() %>% arrange_all()
    out <- rbind(out, cbind(get(paste("c",route,"_bins")) %>% left_join(temp, by = "Logmile"), l[i]))
    out[is.na(out)] <- 0
  }
  colnames(out) <- c("Logmile", "n", paste(t))
  
  #prediction using LOESS | "t"
  #----------------------------------------
  pred <- NULL
  for (i in 1:length(l))
  {
    temp <- out[out[,3]==1,]
    mod <- loess(temp[,2]~temp[,1], span = 0.01)
    pred <- rbind(pred, cbind(predict(mod), frank(-temp[,2], ties.method = "dense"), 
                              frank(-predict(mod), ties.method = "dense"),
                              diff(c(0, temp[,1]))))
  }
  colnames(pred) = c("pred", "rank", "pred.rank", "diff")
  
  all <- data.frame(out, pred) %>% select(Logmile, t, n, pred, rank, pred.rank, diff)
  
  #rank(true counts)|"t", rank(prediction)|"t"
  #------------------------------------------
  t.size <- NULL
  p.size <- NULL
  for (i in 1:length(l))
  {
    t.size <- rbind(t.size, 
                    all[all[,2]==i,] %>% arrange(rank) %>% select(t, rank, n, diff) %>% 
                      group_by(g = get(t), rank) %>% mutate(n_sum = sum(n), size = sum(diff)) %>% 
                      distinct(rank, n_sum, size))
    
    p.size <- rbind(p.size, 
                    all[all[,2]==i,] %>% arrange(pred.rank) %>% 
                      select(t, pred.rank, pred, diff) %>% group_by(g = get(t), pred.rank) %>% 
                      mutate(pred_sum = sum(pred), size = sum(diff)) %>% 
                      distinct(pred.rank, pred_sum, size))
  }
  
  #output will have original counts and predictions, 
  #rank(true counts)|"t", rank(predictions)|"t"
  list(all = all, t.size = t.size, p.size = p.size)
}

test <- crash.results(2015, 71, "Weekend")
a <- test$all
a1 <- test$t.size
a2 <- test$p.size

#plot for original logmile vs. predictions
g <- ggplot(data = a, mapping = aes(x = Logmile, y = n))
g + geom_segment(aes(xend = Logmile, yend = 0, col = factor(Weekend))) + 
  geom_line(aes(x = Logmile, y = pred), col = 4, size = 1.05) + 
  facet_grid(~factor(Weekend))

#size of logmile vs. counts & pred. counts|"t"
for (i in 1:2)
{
  windows()
  plot(cumsum(a1[a1[,2]==i, ]$size), 
       cumsum(a1[a1[,2]==i, ]$n_sum)/sum(a1[a1[,2]==i, ]$n_sum), 
       type = "b", xlab = "size of logmile", ylab = NA,
       main = paste("Weekend","=",i,sep = " "))
  lines(cumsum(a2[a2[,2]==i, ]$size), 
        cumsum(a2[a2[,2]==i,]$pred_sum/sum(a2[a2[,2]==i, ]$pred_sum)),
        lty = 2, col = 4) 
}