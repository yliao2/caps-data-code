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
  out <- matrix(0, length(alpha), 10)
  for (i in 1:length(alpha))
  {
    if(alpha[i]>=1/n)
    {
      for (j in 1:n)
      {
        if((j/n)<=alpha[i]){out[i, 2] <- j/n; out[i, 3] <- j}
        else{out[i, 2] <- out[i, 2]; out[i, 3] <- out[i, 3]}
      }
      out[i, 1] <- alpha[i]
      out[i, 4] <- cumsum(a.rank$p)[out[i, 3]]
      out[i, 5] <- a.rank[out[i, 3],]$p
      out[i, 6] <- cumsum(p.rank$pred.p)[out[i, 3]]
      out[i, 7] <- p.rank[out[i, 3],]$pred.p
      out[i, 8] <- sum(p.rank$pred.p[p.rank$pred.p>=out[i, 5]])
      out[i, 9] <- sum(p.rank$pred.p>=out[i, 5])/n
      out[i,10] <- sum(p.rank$pred.p>=out[i, 5])
    }
    else{out[i, 1] <- alpha[i]; out[i, c(2, 3, 4, 6, 8, 9, 10)] <- 0; out[i, c(5, 7)] <- 1}
  }
  colnames(out) <- c("alpha", "pct.hs", "times", "cum.p", "threshold", 
                     "cum.pred", "pred.threshold", "thres.pred.p", "pct.pred.hs", "pred.times")
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
plot(x = l$out[,"alpha"], y = l$out[,"cum.p"], type = "s", 
     xlab = expression(alpha), ylab = "% events in HS")
lines(x = l$out[,"alpha"], y = l$out[,"cum.pred"], type = "s", col = 4)
legend("right", c("obs.", "pred."), col = c(1, 4), lty = 1)



#% HS above same threshold vs. level alpha (obs. vs. pred.) (modified 2017/10/9)
#-----------------------------------------------------------------------------
plot(x = l$out[, "alpha"], y = l$out[, "pct.hs"], type = "s", xlab = expression(alpha),
     ylab = "% HS")
lines(x = l$out[, "alpha"], y = l$out[, "pct.pred.hs"], type = "s", col = 4)
abline(a = 0, b = 1)
legend("right", c("obs.", "pred."), col = c(1, 4), lty = 1)

plot(x = l$out[, "pct.hs"], y = l$out[, "pct.pred.hs"], type = "s", xlab = "obs. level",
     ylab = "pred. level", col = 4)
abline(a = 0, b = 1, col = "darkgrey")


#add: size of HS vs. threshold
#-----------------------------
plot(x = l$out[-1, "threshold"], y = l$out[-1, c("pct.hs")], type = "s", 
     xlab = "threshold", ylab = "size of hotspot")
lines(x = l$out[-1, "threshold"], y = l$out[-1, c("pct.pred.hs")], type = "s", col = 4)
legend("right", c("obs.", "pred."), col = c(1, 4), lty = 1)


#add: % matched segments
#-------------------------
t1 <- as.data.frame(l$out)
t2 <- l$a.rank
t3 <- l$p.rank
m1 <- NULL
for (i in 1:nrow(t1))
{
  m1 <- c(m1, sum(t2[1:t1$times[i],"Logmile"] %in% t3[1:t1$times[i],"Logmile"])/t1$times[i])
}
plot(x = t1$alpha, y = m1, xlab = expression(alpha), ylab = "% matched logmile/segments",
     type = "s")
abline(h = seq(0, 1, by = 0.2), v = seq(0, 1, by = 0.2), col = "lightgrey")


#add: relative distribution (ratio = obs/pred) in KS test comparing unif(0, 1)
r <- s %>% select(alpha, pct.hs, pct.pred.hs) %>% mutate(r.pct.hs = pct.hs/pct.pred.hs)
n <- length(r$r.pct.hs[is.finite(r$r.pct.hs)])
r1 <- r$r.pct.hs/sum(r$r.pct.hs[is.finite(r$r.pct.hs)])
r2 <- rep(1, n)/n
plot(x = r$alpha, y = r$r.pct.hs, type = "s", xlab = expression(alpha), ylab = "ratio", col = 4)
abline(h = 1, col = "lightgrey")
plot(x = r$alpha[is.finite(r$r.pct.hs)], 
     y = cumsum(r1[is.finite(r1)]), type = "s", xlab = expression(alpha), ylab = NA, col = 4)
lines(x = r$alpha[is.finite(r$r.pct.hs)], y = cumsum(r2), type = "s")
abline(v = seq(0, 1, by = 0.2), h = seq(0, 1, by = 0.2), col = "lightgrey")


#quasi KS test (for Obs. vs. Pred.)
#used for check model fit
#nonparametric statistical inference 4ed, gibbons
#------------------------------------------------
s <- data.frame(l$out)

#quasi KS test function
ks.results <- function(s1, s2, alpha)
{
  n <- length(s1); m <- length(s2)
  c.alpha <- sqrt(-0.5*log(alpha/2))
  bound <- c.alpha*sqrt(1/n+1/m)
  max.d <- max(abs(s1-s2))
  p <- 2*exp(-2*max.d^2/(1/n+1/m))
  if (max.d>=bound){out = paste("Reject H0. max(|s1-s2|) = ", round(max.d, 4), " >= ", 
                                round(bound, 4), " at sig. level = ", alpha, sep = "")}
  else {out = paste("Fail to reject H0. max(|s1-s2|) = ", round(max.d, 4), " < ", 
                    round(bound, 4), " at sig. level = ", alpha, sep = "")}
  list(out = out, max.d = max.d, p.value = p, bound = bound)
}

#quasi KS test for {surveillance plot (level alpha vs. % events in HS). line: 152}
ks.results(s1 = s$cum.p, s2 = s$cum.pred, alpha = 0.05)
ks.results(s1 = s$cum.p, s2 = s$cum.pred, alpha = 0.005)

#quasi KS test for {size of HS vs. threshold. line: 176}
ks.results(s1 = s$pct.hs, s2 = s$pct.pred.hs, alpha = 0.05)

#chi-sqaured test & qks test (for Obs. vs. expected) (uniform)
rel.chisq <- function(obs, pred, alpha)
{
  n <- length(obs)
  d <- n*sum((obs-pred)^2/pred)
  p <- pchisq(d, df = n-1, lower.tail = FALSE)
  bound <- qchisq(1-alpha, df = n-1)
  if(d>=bound){out = paste("Reject H0. chi-sq = ", round(d, 4), " >= ", 
                           round(bound, 4), " at sig. level = ", alpha, sep = "")}
  else{out = paste("Fail to reject H0. chi-sq = ", round(d, 4), " < ", 
                   round(bound, 4), " at sig. level = ", alpha, sep = "")}
  list(out = out, statistics = d, pvalue = p)
}
rel.chisq(obs = r1[is.finite(r1)], pred = r2, alpha = 0.05)
ks.results(s1 = cumsum(r1[is.finite(r1)]), s2 = cumsum(r2), alpha = 0.05)