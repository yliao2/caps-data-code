library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(sqldf)
library(chron)
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




#reference of Logmile given route
ref.logmile <- function(route)
{
  highwaycrash %>% filter(Route == route) %>% select(Logmile) %>% 
    arrange_all() %>% distinct(Logmile) %>% mutate(diff = diff(c(0, Logmile)))
}

#extract data from a given year, route, time
observed.data <- function(year, route, time)
{
  z <- c %>% filter(Year==year & HWY_Route==route) %>% select(time, Logmile) %>% 
    group_by_all() %>% count()
  return(z)
}

#merge ref Logmile (whole original set) to extract data
obs2ref <- function(year, route, time)
{
  r <- ref.logmile(route)
  o <- observed.data(year, route, time)
  lvl <- unique(o[[time]])
  z <- c()
  for (var in lvl)
  {
    assign(paste("o",var), o[which(o[[time]]==var),])
    z <- rbind(z, r %>% left_join(get(paste("o",var)), by = "Logmile"))
    z$n[is.na(z$n)]=0
    z[[time]][is.na(z[[time]])]=var
  }
  return(z)
}

#mod = LOESS
pred.mod <- function(x, y, data)
{
  loess(y~x, data = data)
}

#ks test fn
ks.results <- function(s1, s2, alpha)
{
  n <- length(s1); m <- length(s2)
  c.alpha <- sqrt(-0.5*log(alpha/2))
  bound <- c.alpha*sqrt(1/n+1/m)
  d <- s1-s2
  max.d <- max(abs(s1-s2))
  p <- min(2*exp(-2*max.d^2/(1/n+1/m)), 1)
  if (max.d>=bound){out = paste("Reject H0. max(|s1-s2|) = ", round(max.d, 4), " >= ", 
                                round(bound, 4), " at sig. level = ", alpha, sep = "")}
  else {out = paste("Fail to reject H0. max(|s1-s2|) = ", round(max.d, 4), " < ", 
                    round(bound, 4), " at sig. level = ", alpha, sep = "")}
  list(out = out, max.d = max.d, p.value = p, bound = bound, d = d)
}

#produce rank for each level in <time> var
obs.rank <- function(year, route, time)
{
  o <- obs2ref(year, route, time)
  z <- data.frame(o %>% group_by(get(time)) %>% 
                    mutate(rank = frank(-n, ties.method = "dense"),
                           p = n/sum(n))) %>% select(-get.time.)
  return(z)
}

#count # rank
sum.rank <- function(data)
{
  out <- data %>% select(rank) %>% group_by(rank) %>% count() %>%
    mutate(same.rank = n) %>% select(-n)
  return(out)
}

#level set setting (weekend & weekdays)
z <- obs.rank(2015, 71, "Weekend")
z1 <- z %>% filter(Weekend==1) %>% arrange(rank)
z2 <- z %>% filter(Weekend==2) %>% arrange(rank)

lvl.set <- function(data, alpha)
{
  lvl <- sum.rank(data)
  l <- nrow(lvl)
  n <- sum(lvl$same.rank)
  out <- matrix(0, length(alpha), 5)
  for (i in 1:length(alpha))
  {
    out[i, 1] <- alpha[i]
    if(alpha[i]>lvl$same.rank[1]/n)
    {
      for (j in 1:l)
      {
        if((sum(lvl$same.rank[1:j])/n)<=alpha[i])
        {
          out[i, 2] <- sum(lvl$same.rank[1:j])/n
          out[i, 3] <- sum(lvl$same.rank[1:j])
          out[i, 4] <- sum(data[which(data$rank<=j),]$p)
          out[i, 5] <- min(data[which(data$rank<=j),]$p)
        }
        else{out[i, 2] <- out[i, 2]; out[i, 3] <- out[i, 3];
        out[i, 4] <- out[i, 4]; out[i, 5] <- out[i, 5]}
      }
    }
    else{out[i, 2] <- 0; out[i, 3] <- 0; out[i, 4] <- 0; 
    out[i, 5] <- max(z1$p)}
  }
  colnames(out) <- c("alpha","level", "nseg", "pct.events", "thres")
  return(out)
}

#single alpha = 0.20
#-------------------
alpha = 0.20
t1 <- data.frame(lvl.set(z1, alpha))
t2 <- data.frame(lvl.set(z2, alpha))


plot(z1$Logmile, z1$p, xlab = "Logmile", cex = 0, ylab = "p", main = "weekday")
segments(x0 = z1$Logmile, y0 = z1$p, y1 = 0)
points(x = z1[which(z1$p>=t1$thres), ]$Logmile, y = z1[which(z1$p>=t1$thres), ]$p)
abline(h = t1$thres, col = "darkgray")
text(x = 32, y = t1$thres, labels = round(t1$thres, 4), pos = 3)


plot(z2$Logmile, z2$p, xlab = "Logmile", cex = 0, ylab = "p", main = "weekend")
segments(x0 = z2$Logmile, y0 = z2$p, y1 = 0)
points(x = z2[which(z2$p>=t2$thres), ]$Logmile, y = z2[which(z2$p>=t2$thres), ]$p)
abline(h = t2$thres, col = "darkgray")
text(x = 32, y = t2$thres, labels = round(t2$thres, 4), pos = 3)


#sequence of alphas (alpha = seq(0, 1, length = 11))
#====================================================
#surveillance plot (type 1) (size of distance vs. % events)
#----------------------------------------------------------
z3 <- z1 %>% select(diff, rank, p) %>% group_by(rank) %>% 
  mutate(diff = round(diff, 3), log.size = sum(diff), log.rate = sum(p)) %>% distinct()
z4 <- z2 %>% select(diff, rank, p) %>% group_by(rank) %>% 
  mutate(diff = round(diff, 3), log.size = sum(diff), log.rate = sum(p)) %>% distinct()

plot(x = cumsum(z3$log.size), y = cumsum(z3$log.rate), type = "s", 
     xlab = "size of distance", ylab = "%crashes")
lines(x = cumsum(z4$log.size), y = cumsum(z4$log.rate), type = "s", col = 4)


#surveillance plot (type 2) (level alpha vs. % events in HS)
#--------------------------------------------------
alpha <- seq(0, 1, by = 0.01)
z1.lvl <- data.frame(lvl.set(z1, alpha))
z2.lvl <- data.frame(lvl.set(z2, alpha))

plot(x = z1.lvl$alpha, y = z1.lvl$pct.events, type = "s", 
     xlab = expression(alpha), ylab = "%events in HS")
lines(x = z2.lvl$alpha, y = z2.lvl$pct.events, type = "s", col = 4)
abline(v = 0.3, col = "darkgrey")
text(x = c(0.3, 0.3), 
     y = c(z1.lvl[alpha == 0.3,]$pct.events,z2.lvl[alpha == 0.3,]$pct.events),
     labels = round(c(z1.lvl[alpha == 0.3,]$pct.events, z2.lvl[alpha == 0.3,]$pct.events),4), 
     pos = 3, col = c(1, 4))
legend("bottomright", c("wk", "wkd"), col = c(1, 4), lty = 1)


#ks test for surveillance plot (not significant)
#-----------------------------------------------
t <- ks.results(s1 = z1.lvl$pct.events, s2 = z2.lvl$pct.events, alpha = 0.05)
data.frame(cbind(alpha, d = abs(t$d))) %>% filter(d == t$max.d)

plot(x = alpha, y = abs(t$d), type = "s", xlab = expression(alpha), ylab = "distance")
abline(h = 0, col = "darkgray")

#percentage of crashes in a size/alpha (%) of highest crash rate segments
#ex. alpha = 0.3, proportion of segments id as HS is around 30%, 100% of
#    total crashes happened in these segments during weekend, where as
#    0.9433 of total crashes happened in these segments during weekdays
#
#ks test shows no significance between distributions for weekend & weekdays
#max distance between two distributions happends when alpha = 0.11,
#with max(D) = 0.0851
#--------------------------------------------------------------------------




#proportion/size of highest segements id as HS vs. threshold
#-----------------------------------------------------------
#z1.lvl: level set setting for weekdays (alpha/size, thres, ...)
#z2.lvl: level set setting for weekend (alpha/size, thres, ...)

plot(x = z1.lvl$thres, y = z1.lvl$level, type = "s", 
     xlab = "threshold", ylab = "%segments id as HS", 
     xlim = c(0, max(z1.lvl$thres, z2.lvl$thres)))
lines(x = z2.lvl$thres, y = z2.lvl$level, type = "s", col = 4)
abline(v = 0.007, col = "darkgrey")
text(x = c(0.007, 0.007), y = c(z1.lvl[alpha == 0.01,]$level, z2.lvl[alpha == 0.01,]$level),
     labels = round(c(z1.lvl[alpha == 0.01,]$level, z2.lvl[alpha == 0.01,]$level),4), 
     col = c(1, 4), pos = c(1,3))
legend("topright", c("wk", "wkd"), col = c(1, 4), lty = 1)

#ex. under the same threshold = 0.007, % of segments id as HS (true level)
#    is 0.0098 for weekdays, whereas % of segments id as HS is 0.0082
#    for weekend
#-------------------------------------------------------------------------




#% segments id HS are matched for weekdays & weekend
#---------------------------------------------------
#z1: crash summary weekend==1 in ordered rank of %crashes (weekdays)
#z2: crash summary weekend==2 in ordered rank of %crashes (weekend)
#z : crash summary for both (rbind(z1,z2))
#z1.lvl: level set setting for weekdays (alpha/size, thres, ...)
#z2.lvl: level set setting for weekend (alpha/size, thres, ...)

m1 <- NULL
  for (i in 1:nrow(z1.lvl))
{
  max <- length(unique(c(z1[1:z1.lvl$nseg[i],]$Logmile, 
                         z2[1:z2.lvl$nseg[i],]$Logmile)))
  
  cond <- z1[1:z1.lvl$nseg[i],]$Logmile %in% 
          z2[1:z2.lvl$nseg[i],]$Logmile
  
  m1 <- c(m1, sum(cond)/max)
}
plot(x = alpha, y = m1, type = "s", xlab = expression(alpha), ylab = "% matches")
abline(v = c(0.1, 0.2), col = "darkgrey")
text(x = c(0.1, 0.2), y = m1[c(11, 21)], labels = round(m1[c(11, 21)],4), pos = 3)

#ex. under the same alpha/size/(%segments id HS) = 0.1, 47.93% of segments
#    for weekdays and weekend are matched. if alpha = 0.2, 59.03% of segments
#    given different time period are matched.
#----------------------------------------------------------------------------





#relative distribution plot (compare to unif)
#--------------------------------------------
#z1: crash summary weekend==1 in ordered rank of %crashes (weekdays)
#z2: crash summary weekend==2 in ordered rank of %crashes (weekend)

r <- z1.lvl$pct.events/z2.lvl$pct.events
n <- sum(is.finite(r))
plot(x = alpha, y = r, type = "s", 
     xlab = expression(alpha), 
     ylab = "pct crashes (weekdays)/pct crashes (weekend)")
abline(h = 1, col = "darkgrey")

r1 <- r/sum(r[is.finite(r)])
r2 <- rep(1, n)/n
t3 <- ks.results(s1 = cumsum(r1[is.finite(r1)]), s2 = cumsum(r2), alpha = 0.05)

#convert to uniform cdf
#----------------------
plot(x = alpha[is.finite(r)], y = cumsum(r1[is.finite(r1)]), 
     type = "s", xlab = expression(alpha), ylab = "cumulative ", col = 4)
lines(x = alpha[is.finite(r)], y = cumsum(r2), type = "s")
abline(v = seq(0, 1, by = 0.2), h = seq(0, 1, by = 0.2), col = "lightgrey")

#construct Confidance band
#-------------------------
c.alpha <- sqrt(-0.5*log(0.05/2)) 
bound <- c.alpha*sqrt(1/100+1/100)
low <- cumsum(r1[is.finite(r1)])-bound
up <- cumsum(r1[is.finite(r1)])+bound
low[low<0]=0
up[up>1]=1
lines(x = alpha[is.finite(r)], y = low, type = "s", col = 2)
lines(x = alpha[is.finite(r)], y = up, type = "s", col = 2)


#difference (in distributions) vs. alpha
#and confidence band
#---------------------------------------
ds <- cumsum(r1[is.finite(r1)])-cumsum(r2)
plot(x = alpha[is.finite(r)], y = ds, cex = 0, xlab = expression(alpha),
     ylab = "distance in distribution", ylim = c(-1, 1))
segments(x0 = alpha[is.finite(r)], y0 = ds, y1 = 0)
lines(x = alpha[is.finite(r)], y = rep(bound, 100), type = "s", col = 2)
lines(x = alpha[is.finite(r)], y = rep(-bound, 100), type = "s", col = 2)

#ex. if the probability of crashes given different time (weekend & weekdays)
#    is the same, r, the ratio of % total events within an alpha/size of segments,
#    should be 1 under the same size/alpha.
#------------------------------------------