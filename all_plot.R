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


crash <- read.csv("D:/caps/data/Crash-02-28.csv",sep=",", header=TRUE)
crashreport2 <- read.csv("D:/caps/data/CrashReports2.csv",sep=",", header=TRUE)
highwaycrash <- read.csv("D:/caps/data/HighwayCrash-1-2016.csv",sep=",", header=TRUE)
highway <- read.csv("D:/caps/data/Highway.csv",sep=",", header=TRUE)



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



#produce original counts and predictions, 
#rank(true counts), rank(predictions)
#===============================================
crash.results <- function(year, route)
{
  #logmile from highway on "route" in "year"
  #-------------------------------------------
  assign(paste("c",route,"_bins"), highwaycrash %>% filter(Route==route) %>% select(Logmile) %>% 
           arrange_all() %>% distinct(Logmile))
  
  #merge crash("route","year")
  #------------------------------------------
  temp <- c %>% filter(Year==year & HWY_Route==route) %>% select(Logmile) %>% 
    group_by_all() %>% count() %>% arrange_all()
  out <- get(paste("c",route,"_bins")) %>% left_join(temp, by = "Logmile")
  out[is.na(out)] <- 0
  colnames(out) <- c("Logmile", "n")
  
  #prediction using LOESS
  #----------------------------------------
  mod <- loess(out[,2]~out[,1], span = 0.1)
  pred <- cbind(predict(mod), frank(-out[,2], ties.method = "dense"), 
                              frank(-predict(mod), ties.method = "dense"),
                              diff(c(0, out[,1])))
  colnames(pred) <- c("pred", "rank", "pred.rank", "diff")
  
  all <- data.frame(out, pred) %>% select(Logmile, n, pred, rank, pred.rank, diff)
  
  #rank(true counts)|"t", rank(prediction)|"t"
  #------------------------------------------
  t.size <- all %>% arrange(rank) %>% select(rank, n, diff)
  p.size <- all %>% arrange(pred.rank) %>% select(pred.rank, pred, diff)
  
  #output will have original counts and predictions, 
  #rank(true counts), rank(predictions)
  list(all = all, t.size = t.size, p.size = p.size)
}
test <- crash.results(2015, 71)

a <- test$all %>% mutate(p = n/sum(n), pred.p = pred/sum(pred), ratio.p = p/pred.p)


#function for level sets
#alpha = % of road in HS
#================================================
level.results <- function(alpha)
{
  a.rank <- test$all %>% mutate(p = n/sum(n), pred.p = pred/sum(pred), ratio.p = p/pred.p) %>% arrange(rank)
  p.rank <- a.rank %>% arrange(pred.rank)
  n <- nrow(a.rank)
  out <- matrix(0, length(alpha), 8)
  for (i in 1:length(alpha))
  {
    if(alpha[i]>=1/n)
    {
      for (j in 1:n)
      {
        if((j/n)<=alpha[i]){out[i, 2] <- j/n; out[i, 3] <- j; out[]}
        else{out[i, 2] <- out[i, 2]; out[i, 3] <- out[i, 3]}
      }
      out[i, 1] <- alpha[i]
      out[i, 4] <- cumsum(a.rank$p)[out[i, 3]]
      out[i, 5] <- a.rank[out[i, 3],]$p
      out[i, 6] <- cumsum(p.rank$pred.p)[out[i, 3]]
      out[i, 7] <- p.rank[out[i, 3],]$pred.p
      out[i, 8] <- sum(p.rank$pred.p[p.rank$pred.p>=out[i, 5]])
    }
    else{out[i, 1] <- alpha[i]; out[i, c(2, 3, 4, 6, 8)] <- 0; out[i, c(5, 7)] <- 1}
  }
  colnames(out) <- c("alpha", "pct.hs", "times", "cum.p", "threshold", 
                     "cum.pred", "pred.threshold", "pct.pred.hs")
  list(out = out, a.rank = a.rank, p.rank = p.rank)
}

#single alpha (alpha = 0.06)
#plot for observations & predictions based on level alpha
#========================================================
l1 <- level.results(0.06)
plot(a$Logmile, a$p, xlab = "Logmile", cex = 0, ylab = "p")
lines(x = a$Logmile, y = a$pred.p, col = 4, lwd = 2)
segments(x0 = a$Logmile, y0 = a$p, y1 = 0)

points(l1$a.rank[1:l1$out[1,3],]$Logmile, l1$a.rank[1:l1$out[1,3],]$p, col = 2)
abline(h = l1$out[1,5], col = 2)
text(x = max(l1$a.rank$Logmile)-1, y = l1$out[1,5], label = round(l1$out[1,5], 4), pos = 3, col = 2)

points(l1$p.rank[1:l1$out[1,3],]$Logmile, l1$p.rank[1:l1$out[1,3],]$pred.p, col = 4)
abline(h = l1$out[1,7], col = 4)
text(x = max(l1$p.rank$Logmile)-1, y = l1$out[1,7], label = round(l1$out[1,7], 4), pos = 3, col = 4)

legend("topright", c("obs.", "pred.", "id obs. HS", "id pred. HS"), 
       col = c(1, 4, 2, 4), lty = c(1, 1, NA, NA), lwd = c(1, 2, NA, NA), pch = c(NA, NA, 1, 1))



#sequence of alphas (alpha = seq(0, 1, length = 101))
#====================================================

#surveillance plot (level alpha vs. % events in HS)
#--------------------------------------------------
alpha <- seq(0, 1, length = 101)
l <- level.results(alpha)
plot(x = l$out[,c("alpha")], y = l$out[,c("cum.p")], type = "s", xlab = expression(alpha),
     ylab = "% events in HS")
lines(x = l$out[,c("alpha")], y = l$out[,c("cum.pred")], type = "s", col = 4)
legend("right", c("obs.", "pred."), col = c(1, 4), lty = 1)

#% HS above threshold (based on observations) vs. level alpha (obs. vs. pred.)
#-----------------------------------------------------------------------------
plot(x = l$out[,c("alpha")], y = l$out[,c("pct.hs")], type = "s", xlab = expression(alpha),
     ylab = "% HS")
lines(x = l$out[, c("alpha")], y = l$out[,c("pct.pred.hs")], type = "s", col = 4)
abline(a = 0, b = 1)
legend("right", c("obs.", "pred."), col = c(1, 4), lty = 1)




#similar KS results
#------------------
head(l$out)
ks.results <- a %>% select(Logmile, p, pred.p) %>% 
  mutate(s.p = cumsum(p), s.pred.p = cumsum(pred.p), d = abs(s.p-s.pred.p))
sig.level <- cbind(alpha = c(0.1, 0.05, 0.025, 0.01, 0.005, 0.001),
                   value = c(1.22, 1.36, 1.48, 1.63, 1.73, 1.95))
bound <- sig.level[, 2]*sqrt((2*nrow(a)/(nrow(a)^2)))
max(ks.results$d)



#####alpha = % events above threshold
#single.level <- function(v)
#{
#  windows()
#  plot(a$Logmile, a$p, xlab = "Logmile", cex = 0, ylab = "p")
#  segments(x0 = a$Logmile, y0 = a$p, y1 = 0)
#  lines(x = a$Logmile, y = a$pred.p, col = 4, lwd = 2)
#  
#  points(l$a.rank[1:l$out[v,3],]$Logmile, l$a.rank[1:l$out[v,3],]$p, col = 2)
#  abline(h = l$out[v,5], col = 2)
#  text(x = max(l$a.rank$Logmile)-1, y = l$out[v,5], label = round(l$out[v,5], 4), pos = 3, col = 2)
#  
#  points(l$p.rank[1:l$out[v,7],]$Logmile, l$p.rank[1:l$out[v,7],]$pred.p, col = 4)
#  abline(h = l$out[v,9], col = 4)
#  text(x = max(l$p.rank$Logmile)-1, y = l$out[v,9], label = round(l$out[v,9], 4), pos = 3, col = 4)
#}
#single.level(30)
#
####sequence of alpha
#colnames(l$out)

#windows()
#plot(x = l$out[,c("pct.hs")], y = l$out[,c("alpha")], type = "s", 
#     xlab = "% hotspot", ylab = expression(alpha))
#lines(x = l$out[,c("pre.pct.hs")], y = l$out[,c("alpha")], type = "s", col = 4)
#
#windows()
#plot(x = l$out[,c("pct.hs")], y = l$out[,c("cum.p")], type = "s")
#lines(x = l$out[,c("pre.pct.hs")], y = l$out[,c("cum.pred")], type = "s", col = 4)