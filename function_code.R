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
crash <- read.csv("G:/caps/data/Crash-02-28.csv",sep=",", header=TRUE)
crashreport2 <- read.csv("G:/caps/data/CrashReports2.csv",sep=",", header=TRUE)
highwaycrash <- read.csv("G:/caps/data/HighwayCrash-1-2016.csv",sep=",", header=TRUE)
highway <- read.csv("G:/caps/data/Highway.csv",sep=",", header=TRUE)
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
ks.results <- function(s1, s2, alpha)
{
  n <- length(s1); m <- length(s2)
  c.alpha <- sqrt(-0.5*log(alpha/2))
  bound <- c.alpha*sqrt(1/n+1/m)
  d <- s1-s2
  max.d <- max(abs(s1-s2))
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
origin.pair <- function(x, alpha, var)
{
  a <- get(x[1])
  b <- get(x[2])
  if(sum(a[[var]])==1)
    {out <- ks.results(s1 = cumsum(a[[var]]), s2 = cumsum(b[[var]]), alpha = alpha)}
  else
    {out <- ks.results(s1 = a[[var]], s2 = b[[var]], alpha = alpha)}
  return(out)
}
#---------------------------------------------------------



#fn. for ks result for multiple comparison
#(data has to include all subsets)
#-----------------------------------------
pair.data <- function(data, tag, time, var, alpha)
{
  #combination of tag (for paired comparisons)
  comb <- combn(x = tag, m = 2)
  
  for (i in 1:length(tag))
  {assign(tag[i], data[which(data[[time]]==i),], inherits = TRUE)}
  
  #ks results for each pair
  out <- NULL
  for (i in 1:ncol(comb))
  {test <- origin.pair(comb[,i], alpha = alpha, var = var)
   out <- rbind(out, round(c(test$max.d, test$p.value), 4))}
  colnames(out) <- c("D", "p.value")
  data.frame(Reference = t(comb)[,1], Compare = t(comb)[,2], out)
}
#------------------------------------------



#find % matched segments for every combination of "tag"
#(ex. sun - mon, sun - tue, sun - wed, ...)
#------------------------------------------
pair.match <- function(tag, alpha)
{
  comb <- combn(x = tag, m = 2)
  m1 <- NULL
  for (i in 1:ncol(comb))
  {
    a <- get(comb[1, i])
    b <- get(comb[2, i])
    
    n1 <- out[out$time==which(tag==comb[1,i]),]$nseg
    n2 <- out[out$time==which(tag==comb[2,i]),]$nseg
    
    max <- length(unique(c(a[1:n1,]$Logmile, b[1:n2,]$Logmile)))
    cond <- a[1:n1,]$Logmile %in% b[1:n2,]$Logmile
    
    m1 <- c(m1, sum(cond)/max)
  }
  data.frame(t(comb), match = m1)
}
#------------------------------------------



#read lines from source
#-----------------------------------------------
source_lines <- function(file, lines)
{source(textConnection(readLines(file)[lines]))}
#-----------------------------------------------



#modification of jaccard index:
#= intersection(reference, compare)/(reference: model)
#------------------------------------------------------------
data2pred <- function(ref, compr, var)
{
  x <- ref
  y <- compr
  m <- length(unique(x$rank))
  
  match <- NULL
  nmatch <- NULL
  for (i in 1:m)
  {
    x1 <- x[which(x$rank<=i), ][[var]]
    y1 <- y[which(y$rank<=i), ][[var]]
    cond <- y1 %in% x1
    match <- c(match, sum(cond)/length(x1))
    nmatch <- c(nmatch, sum(cond))
  }
  data.frame(ref.rank = 1:m, match = match, nmatch = nmatch)
}
#------------------------------------------------------------