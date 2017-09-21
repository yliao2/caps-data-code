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



#counts of crashes by Year, Route
route_n <- c %>% group_by(Year, HWY_Route) %>% count() %>% arrange(Year, desc(n))


#use route 71 in 2015
route <- function(year, num)
{
  mydata <- subset(c, Year==year & HWY_Route %in% num)
  return(mydata)
}

c71 <- route(2015, 71)
c49 <- route(2015, 49)
c30 <- route(2015, 30)



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
pred.crash2(c71, "Weekend", 50)





#9/21
#density and rank for logmile
hist(c71$Logmile, nclass = 35, prob = TRUE)
lines(density(c71$Logmile, kernel = "gaussian", from = 0,
              width = 1, n = 30), col = 4)
rug(c71$Logmile)

kdplot <- function(var, bins, method)
{
  true <- binning(x = var, breaks = seq(0, max(var), max(var)/bins))
  est <- density(var, kernel = method, from = true$breaks[2], to = max(true$breaks),
                 cut = max(var)/bins, n = bins)
  plot(true, main = NA, xlab = "Logmile")
  lines(est, col = 4)
  t.rk <- rank(-true$counts)
  e.rk <- rank(-est$y)
  t1 <- NULL
  for (i in 1:bins)
  {
    cond <- t.rk[1:i] %in% e.rk[1:i]
    t1 <- c(t1, sum(cond)/i)
  }  
  data.frame(list(mids = true$mids, counts = true$counts, est.density = est$y, 
                  true.rk = t.rk, est.rk = e.rk, match = t1))
}
a <- kdplot(c71$Logmile, 30, "rectangular")



######
plot(a$mids, cumsum(a$counts)/sum(a$counts), xlab = NA, ylab = NA, xlim = c(0,35), ylim = c(0,1))
r <- cumsum(a$counts)/sum(a$counts)
shape <- seq(1, 5, 1)
rate <- seq(1, 5, 1)
test <- matrix(0, 5, 5)
for (i in 1:length(shape))
{
  for (j in 1:length(rate))
  {
    t <- cumsum(dgamma(a$mids, shape = i, rate = j))
    test[i, j] <- sum((t-r)^2)
  }
}
min(test) #shape = 4, rate = 1
lines(a$mids, rank(-dgamma(a$mids, shape = 4, rate = 1)))
for (i in 1:5)
{
  for (j in 1:5)
  {
    lines(a$mids, cumsum(dgamma(a$mids, shape = i, rate = j)))
  }
}
plot(seq(1,10,0.1), 1-dgamma(seq(1,10,0.1), shape = 1, rate = 1), type = "l")



#test for surveillance plot
a1 <- binning(x = c71[which(c71$Weekend==1),]$Logmile, 
             breaks = seq(0, max(c71$Logmile), max(c71$Logmile)/30))
a2 <- binning(x = c71[which(c71$Weekend==2),]$Logmile, 
              breaks = seq(0, max(c71$Logmile), max(c71$Logmile)/30))
p1 <- predict(loess(a1$counts/sum(a1$counts)~a1$mids))
p2 <- predict(loess(a2$counts/sum(a1$counts)~a2$mids))
p <- predict(loess(a1$counts+a2$counts~a1$mids))
rank.t <- data.frame(bins = 1:30, mid = a1$mids, 
                     wk.n = a1$counts,
                     wk.pn = a1$counts/sum(a1$counts),
                     wk.rk = rank(-a1$counts, ties.method = "first"),
                     wk.p.n = p1,
                     wk.p.pn = p1/sum(p1),
                     wk.p.rk = rank(-p1, ties.method = "first"),
                     wkd.n = a2$counts,
                     wkd.pn = a2$counts/sum(a2$counts),
                     wkd.rk = rank(-a2$counts, ties.method = "first"),
                     wkd.p.n = p2,
                     wkd.p.pn = p2/sum(p2),
                     wkd.p.rk = rank(-p2, ties.method = "first"))

plot(1:30, cumsum(arrange(rank.t, wk.rk)$wk.pn), xlab = "size of rank hotspot", 
     ylab = "cumulative crash rate", xlim = c(0, 30), ylim = c(0, 1), 
     type = "l", main = "crashes on route 71")
lines(1:30, cumsum(arrange(rank.t, wk.p.rk)$wk.p.pn), 
      xlim = c(0, 30), ylim = c(0, 1), col = 2)
lines(1:30, cumsum(arrange(rank.t, wkd.rk)$wkd.pn), 
      xlim = c(0, 30), ylim = c(0, 1), col = 3)
lines(1:30, cumsum(arrange(rank.t, wkd.p.rk)$wkd.p.pn), 
      xlim = c(0, 30), ylim = c(0, 1), col = 4)
text(x = 1:30, y = cumsum(arrange(rank.t, wk.rk)$wk.pn),
     labels = arrange(rank.t, wk.rk)$bins, col = 1, pos = 3)
text(x = 1:30, y = cumsum(arrange(rank.t, wk.p.rk)$wk.p.pn),
     labels = arrange(rank.t, wk.p.rk)$bins, col = 2, pos = 1)
text(x = 1:30, y = cumsum(arrange(rank.t, wkd.rk)$wkd.pn),
     labels = arrange(rank.t, wkd.rk)$bins, col = 3, pos = 3)
text(x = 1:30, y = cumsum(arrange(rank.t, wkd.p.rk)$wkd.p.pn),
     labels = arrange(rank.t, wkd.p.rk)$bins, col = 4, pos = 1)
abline(h=seq(0,1, by=.10), col="lightgray")
abline(v=seq(0, 30, by=5), col="lightgray")


#rank table
d <- data.frame(rank = 1:30, 
                wk.rk = arrange(rank.t, wk.rk)$bins, 
                wk.p.rk = arrange(rank.t, wk.p.rk)$bins,
                wkd.rk = arrange(rank.t, wkd.rk)$bins,
                wkd.p.rk = arrange(rank.t, wkd.p.rk)$bins,
                all = rank(-(a1$counts+a2$counts)),
                all.p = rank(-p))
t1 <- NULL
t2 <- NULL
t3 <- NULL
for (i in 1:30)
{
  cond1 <- d$wk.p.rk[1:i] %in% d$wk.rk[1:i]
  t1 <- c(t1, sum(cond1)/i)
  cond2 <- d$wkd.p.rk[1:i] %in% d$wkd.rk[1:i]
  t2 <- c(t2, sum(cond2)/i)
  cond3 <- d$all.p[1:i] %in% d$all[1:i]
  t3 <- c(t3, sum(cond3)/i)
}
t <- cbind(d, wk.match = t1, wkd.match = t2, all.match = t3)
plot(t$rank, t$wk.match, type = "l")
lines(t$rank, t$wkd.match, col = 2)




#subset weekend
n <- nrow(c71)
a1 <- binning(x = c71[which(c71$Weekend == 1), ]$Logmile, 
              breaks = seq(0, max(c71$Logmile), max(c71$Logmile)/30))
a2 <- binning(x = c71[which(c71$Weekend == 2), ]$Logmile, 
              breaks = seq(0, max(c71$Logmile), max(c71$Logmile)/30))
p1 <- loess(a1$counts~a1$mids)
p2 <- loess(a2$counts~a2$mids)

bins.table <- data.frame(
cbind(bins = 1:30, mid = a1$mids, wk = a1$counts, wk.ord = rank(-a1$counts), 
      wk.pred = predict(p1, newdata = a1$mids), wk.pred.ord = rank(-predict(p1, newdata = a1$mids)), 
      wkd = a2$counts, wkd.ord = rank(-a2$counts), wkd.pred = predict(p2, newdata = a2$mids), 
      wkd.pred.ord = rank(-predict(p2, newdata = a2$mids))))

plot(bins.table$bins-0.1, bins.table$wk, col = 1, pch = 16, cex = 0.7, xlab = NA, ylab = NA)
points(bins.table$bins+0.1, bins.table$wkd, col = 2, pch = 16, cex = 0.7)
segments(x0 = bins.table$bins-0.1, y0 = bins.table$wk, y1 = 0, col = 1)
segments(x0 = bins.table$bins+0.1, y0 = bins.table$wkd, y1 = 0, col = 2)
text(x = bins.table$bins-0.1, y = bins.table$wk, labels = bins.table$wk.ord, col = 1, pos = 3)
text(x = bins.table$bins+0.1, y = bins.table$wkd, col = 2, labels = bins.table$wkd.ord, pos = 3)












#test of variances & means for Weekend==1, 0
test <- c71 %>% select(CrashDate) %>% group_by(CrashDate) %>% 
  count() %>% mutate(Weekend = ifelse(day.of.week(month(CrashDate), day(CrashDate), year(CrashDate)) %in% 
                                        c(1, 2, 3, 4, 5), 1, 2))
test %>% ggplot(aes(n)) + geom_density(aes(y=..count.., col = factor(Weekend)))

#Kolmogorov-Smirnov test
test1 <- ks.test(test[test$Weekend==1,]$n,test[test$Weekend==2,]$n)

#empirical CDF
a <- test[test$Weekend==1, ]$n
b <- test[test$Weekend==2, ]$n
ecdf.x <- ecdf(a)
ecdf.y <- ecdf(b)
plot(ecdf.x, xlim = c(min(c(a, b)), max(c(a, b))), main = "Empirical CDF")
plot(ecdf.y, add = TRUE, col = 2)


a1 <- c71[c71$Weekend==1, ]$Logmile
b1 <- c71[c71$Weekend==2, ]$Logmile
ecdf.x1 <- ecdf(a1)
ecdf.y1 <- ecdf(b1)
plot(ecdf.x1, xlim = c(min(c(a1, b1)), max(c(a1, b1))), main = "Empirical CDF")
plot(ecdf.y1, add = TRUE, col = 2)








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

