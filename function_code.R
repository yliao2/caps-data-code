#=========================================
#----------   packages used   ------------
#=========================================
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(sqldf)
library(chron)
library(data.table)
library(bda)




#=========================================
#--------   data sources parts   ---------
#=========================================

#---------------------------------------------------------------------------
crash <- read.csv("D:/caps/data/Crash-02-28.csv",sep=",", header=TRUE)
crashreport2 <- read.csv("D:/caps/data/CrashReports2.csv",sep=",", header=TRUE)
highwaycrash <- read.csv("D:/caps/data/HighwayCrash-1-2016.csv",sep=",", header=TRUE)
highway <- read.csv("D:/caps/data/Highway.csv",sep=",", header=TRUE)
#---------------------------------------------------------------------------

#use highwaycrash & crash to merge into c
#add: different groups of time of day
#-----------------------------------------------------------------------------
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
#----------------------------------------------------------------------------


#======================================
#--------   all functions   -----------
#======================================

#reference of Logmile given route
#--------------------------------
ref.logmile <- function(route)
{
  highwaycrash %>% filter(Route == route) %>% select(Logmile) %>% 
    arrange_all() %>% distinct(Logmile) %>% mutate(diff = diff(c(0, Logmile)))
}
#---------------------------------



#extract data from a given year, route, time
#-------------------------------------------
observed.data <- function(year, route, time)
{
  z <- c %>% filter(Year==year & HWY_Route==route) %>% select(time, Logmile) %>% 
    group_by_all() %>% count()
  return(z)
}
#-------------------------------------------



#merge ref Logmile (whole original set) to extract data
#------------------------------------------------------
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
#-------------------------------------------------------



#ks test fn
#-------------------------------------------
ks.results <- function(s1, s2, alpha, lvl = TRUE)
{
  n <- length(s1); m <- length(s2)
  c.alpha <- sqrt(-0.5*log(alpha/2))
  bound <- c.alpha*sqrt(1/n+1/m)
  d <- s1-s2
  max.d <- ifelse(lvl==TRUE, max(abs(d)), max(abs(d[1:min(n,m)])))
  p <- min(2*exp(-2*max.d^2/(1/n+1/m)), 1)
  if (max.d>=bound){out = paste("Reject H0. D.max = ", round(max.d, 4), " >= ", 
                                round(bound, 4), " sig. level = ", alpha, sep = "")}
  else {out = paste("Fail to reject H0. D.max = ", round(max.d, 4), " < ", 
                    round(bound, 4), " sig. level = ", alpha, sep = "")}
  list(out = out, max.d = max.d, p.value = p, bound = bound, d = d)
}
#--------------------------------------------



#produce rank for each level in <time> var
#--------------------------------------------
obs.rank <- function(year, route, time)
{
  o <- obs2ref(year, route, time)
  z <- data.frame(o %>% group_by(get(time)) %>% 
                    mutate(rank = frank(-n, ties.method = "dense"),
                           p = n/sum(n))) %>% select(-get.time.)
  return(z)
}
#---------------------------------------------



#count # rank
#---------------------------------------------
sum.rank <- function(data)
{
  out <- data %>% select(rank) %>% group_by(rank) %>% count() %>%
    mutate(same.rank = n) %>% select(-n)
  return(out)
}
#---------------------------------------------



#level set
#--------------------------------------
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
    out[i, 5] <- max(data$p)}
  }
  colnames(out) <- c("alpha","level", "nseg", "pct.events", "thres")
  return(out)
}
#--------------------------------------



#fn for threshold based surveillance
#(a seq of thresholds)
#-----------------------------------
thres.out <- function(data, var, thr)
{
  d <- data
  n <- nrow(d)
  v <- var
  o <- d[v]
  out <- NULL
  for (i in 1:length(thr)) {out <- c(out, sum(o>=thr[i]))}
  return(data.frame(thr, nseg = out, size = out/n))
}
#-----------------------------------



#binning and create rank & p for each bins
#-----------------------------------------
bin2rank <- function(x, counts, breaks)
{
  bins <- binning(x = x, counts = counts, breaks = breaks)
  out <- data.frame(breaks = bins$breaks[-1], 
                    mids = bins$mids, 
                    counts = bins$counts, 
                    rank = frank(-bins$counts, ties.method = "dense")) %>% 
    arrange(rank) %>% mutate(p = counts/sum(counts))
  return(out)
}
#-----------------------------------------



#subset "data" into "tag" given "time"
#bins are ordered
#-----------------------------------------
bin2rank.time <- function(data, time, tag, brks)
{
  for (var in tag)
  {
    x <- data %>% filter(get(time)==which(tag==var))
    assign(var, bin2rank(x = x$Logmile, counts = x$n, breaks = brks), 
           inherits = TRUE)
  }
}
#------------------------------------------



#subset "data" into "tag" given "time"
#in a ranked order (implicit)
#-----------------------------------------
rank.time.dt <- function(data, tag, time)
{
  for (var in tag)
  {assign(var, 
          data %>% filter(get(time)==which(tag==var)) %>% arrange(rank), 
          inherits = TRUE)}
}
#-----------------------------------------



#create lvl set results from 
#subset "data" from "time"
#-----------------------------------------
lvl.set.time <- function(tag, alpha)
{
  for (var in tag)
  {
    out <- NULL
    for (i in 1:length(alpha))
    {
      d <- data.frame(lvl.set(data = get(var), alpha = alpha[i]),
                      time = which(tag==var))
      out <- rbind(out, d)
    }
    assign(paste(var,".lvl",sep = ""), out, inherits = TRUE)
  }
}
#-----------------------------------------



#fn. for paired comparison using ks test
#(x = which groups to compare (based on combn function), 
# alpha = significance level)
#---------------------------------------------------------
origin.pair <- function(ref, compare, sig = 0.05, var = "Logmile", opt=TRUE)
{
  l <- as.matrix(expand.grid(list(ref, compare)))
  n <- nrow(l)
  out1 <- NULL
  out2 <- NULL
  for (i in 1:n)
  {
    a <- get(l[i,1]); a.lvl <- get(paste(l[i,1],".lvl",sep=""))
    b <- get(l[i,2]); b.lvl <- get(paste(l[i,2],".lvl",sep=""))
    
    l1 <- NULL
    if(opt==TRUE)
    {
      for (j in 1:length(a.lvl$alpha))
      {
        l1 <- c(l1, pred.rank(ref = a[0:a.lvl$nseg[j],], 
                              compare = b[0:b.lvl$nseg[j],], var = var, rank = FALSE))
      }
      k <- ks.results(s1 = a.lvl$pct.events, s2 = l1, alpha = sig)
    }
    else
    {
      l1 <- pred.rank(ref = a, compare = b)$cum.match.pct
      true1 <- a %>% select(rank, p) %>% group_by(rank) %>% mutate(sum.p = sum(p)) %>% distinct()
      k <- ks.results(s1 = cumsum(true1$sum.p), s2 = l1, alpha = sig, lvl=FALSE)
    }
  
    out1 <- c(out1, k$max.d)
    out2 <- c(out2, k$p.value)
    out3 <- k$bound
  }
  m1 <- round(matrix(out1, ncol = length(ref), 
                     byrow = TRUE, dimnames = list(ref,compare)),4)
  m2 <- round(matrix(out2, ncol = length(ref), 
                     byrow = TRUE, dimnames = list(ref,compare)),4)
  list(max.d = m1, p.value = m2, bound = out3)
}
#---------------------------------------------------------



#read lines from source
#-----------------------------------------------
source_lines <- function(file, lines)
{source(textConnection(readLines(file)[lines]))}
#-----------------------------------------------



#modification of jaccard index:
#= intersection(reference, compare)/(reference: model)
#------------------------------------------------------------
data2pred <- function(ref, compare, var = "Logmile", alpha = seq(0,1,by=0.01))
{
  l <- as.matrix(expand.grid(list(ref, compare)))
  n <- nrow(l)
  match.rate <- alpha
  for (i in 1:n)
  {
    a <- get(l[i,1]); a.lvl <- get(paste(l[i,1],".lvl",sep=""))
    b <- get(l[i,2]); b.lvl <- get(paste(l[i,2],".lvl",sep=""))
    m <- nrow(a.lvl)
    match <- NULL
    nmatch <- NULL
    for (j in 1:m)
    {
      a1 <- a[0:a.lvl$nseg[j],][[var]]
      b1 <- b[0:b.lvl$nseg[j],][[var]]
      cond <- b1 %in% a1
      match <- c(match, sum(cond)/length(a1))
    }
    match.rate <- cbind(match.rate, match)
  }
  colnames(match.rate) <- c("alpha",compare)
  data.frame(match.rate)
}
#------------------------------------------------------------



#-------------------------------------------------------------------------------
pred.rank <- function(ref, compare, var = "Logmile", rank=TRUE)
{
  a = NULL
  if (rank==TRUE)
  {
    u = unique(ref$rank)
    for (i in 1:length(u))
    {a = c(a, sum(compare[compare[[var]] %in% ref[ref$rank<=i,][[var]],]$p))}
    data.frame(ref.rank = u, cum.match.pct = a)
  }
  else
  {
    a = sum(compare[compare[[var]] %in% ref[[var]],]$p)
    return(a)
  }
}
#-------------------------------------------------------------------------------